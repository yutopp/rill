(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Stdint
module L = Llvm
module LBW = Llvm_bitwriter

module TAst = Tagged_ast
type env_t = TAst.t Env.env_t
type type_info_t = env_t Type.info_t
type ctfe_val_t = type_info_t Ctfe_value.t
type type_gen_t = env_t Type.Generator.t

type value_form_t = bool    (* is_address or not *)

type builtin_func_params_t =
    (type_info_t * value_form_t) list
type builtin_func_ret_t =
    (type_info_t * value_form_t)
type 'ctx builtin_func_def_t =
    builtin_func_params_t -> builtin_func_ret_t -> L.llvalue array -> 'ctx -> L.llvalue
type ('ty, 'ctx) builtin_template_func_def_t =
    'ty Ctfe_value.t list ->
    builtin_func_params_t -> builtin_func_ret_t -> L.llvalue array -> 'ctx -> L.llvalue

type ('ty, 'ctx) record_value_t =
  | LLValue of (L.llvalue * value_form_t)
  | LLType of L.lltype
  | LLTypeGen of ('ty Ctfe_value.t list -> L.lltype)
  | ElemIndex of int
  | BuiltinFunc of 'ctx builtin_func_def_t
  | BuiltinFuncGen of ('ty, 'ctx) builtin_template_func_def_t
  | TrivialAction

module CodeGeneratorType =
  struct
    type ir_context_t = L.llcontext
    type ir_builder_t = L.llbuilder
    type ir_module_t = L.llmodule
    type ir_intrinsics = Codegen_llvm_intrinsics.t
    type 'ty ir_cache_value_t = L.llvalue * 'ty * bool

    type ('ty, 'ctx) value_t = ('ty, 'ctx) record_value_t
  end
module Ctx = Codegen_context.Make(CodeGeneratorType)
type ctx_t = (env_t, Nodes.CachedNodeCounter.t, type_info_t, ctfe_val_t) Ctx.t

let agg_recever_name = "agg.receiver"

let make_default_context ~type_sets ~uni_map ~target_module =
  let ir_context = L.global_context () in
  let ir_module = L.create_module ir_context "Rill" in
  let ir_builder = L.builder ir_context in

  let ir_intrinsics =
    Codegen_llvm_intrinsics.declare_intrinsics ir_context ir_module
  in

  Ctx.init
    ~ir_context:ir_context
    ~ir_builder:ir_builder
    ~ir_module:ir_module
    ~ir_intrinsics:ir_intrinsics
    ~type_sets:type_sets
    ~uni_map:uni_map
    ~target_module_id:(target_module |> Option.map Env.get_id)


let llval_i1 v ctx =
  let open Ctx in
  L.const_int (L.i1_type ctx.ir_context) (if v then 1 else 0)

let llval_i32 v ctx =
  let open Ctx in
  L.const_int_of_string (L.i32_type ctx.ir_context) (Int32.to_string v) 10

let llval_u32 v ctx =
  let open Ctx in
  L.const_int_of_string (L.i32_type ctx.ir_context) (Uint32.to_string v) 10

let do_debug_print_flag = not Config.is_release && false

let debug_dump_value v =
  if do_debug_print_flag then
    Debug.printf "%s\n" (L.string_of_llvalue v)

let debug_dump_module m =
  if do_debug_print_flag then
    Debug.printf "%s\n" (L.string_of_llmodule m)

let debug_dump_type t =
  if do_debug_print_flag then
    Debug.printf "%s\n" (L.string_of_lltype t)

type cg_aux_t =
    {
      cg_returns_addr : bool;
      cg_has_terminator: bool;
    }

let rec generate_code ?(storage=None) node ctx : (L.llvalue * 'env Type.info_t * bool) =
  let open Ctx in
  let void_v = L.undef (L.void_type ctx.ir_context) in
  let void_ty = ctx.type_sets.Type_sets.ts_void_type in
  let void_val = (void_v, void_ty, false) in

  match node with
  | TAst.Module (inner, pkg_names, mod_name, base_dir, _) ->
     generate_code inner ctx

  | TAst.StatementList (nodes) ->
     let rec gen nx (v, t, p) = match nx with
       | [] -> (v, t, p)
       | x :: xs ->
          let l = generate_code x ctx in
          gen xs l
     in
     gen nodes void_val

  | TAst.ExprStmt e ->
     generate_code e ctx

  | TAst.VoidExprStmt e ->
     let _ = generate_code e ctx in
     (void_v, void_ty, false)

  | TAst.ReturnStmt (opt_e) ->
     begin
       Debug.printf "ReturnStmt!!!!!!!!\n";

       let llval = match opt_e with
         | Some e ->
            let (llval, ty, is_addr) = generate_code e ctx in
            if is_heavy_object ty || ty == void_ty then
              L.build_ret_void ctx.ir_builder
            else
              let llval = adjust_addr_val llval ty is_addr ctx in
              L.build_ret llval ctx.ir_builder
         | None ->
            L.build_ret_void ctx.ir_builder
       in

       (llval, void_ty, false)
     end

  | TAst.GenericFuncDef (opt_body, Some env) ->
     if Ctx.is_env_defined ctx env then void_val else
     begin
       Ctx.mark_env_as_defined ctx env;

       let fenv_r = Env.FunctionOp.get_record env in
       Debug.printf "\n\n\n\n<><><><>\nDefine: %s\n<><><><>\n%s\n\n\n\n" (fenv_r.Env.fn_mangled |> Option.get) (Env_system.EnvId.to_string env.Env.env_id);

       let declare_current_function name =
         (* if this class returns non primitive object,
          * add a parameter to receive the object *)
         let returns_heavy_obj = is_heavy_object fenv_r.Env.fn_return_type in

         let func_ret_ty =
           if returns_heavy_obj then
             ctx.type_sets.Type_sets.ts_void_type
           else
             fenv_r.Env.fn_return_type
         in
         let llret_ty = lltype_of_typeinfo_ret func_ret_ty ctx in

         let (_, llparam_tys) =
           paramkinds_to_llparams fenv_r.Env.fn_param_kinds
                                  fenv_r.Env.fn_return_type
                                  ctx
         in

         (* type of function *)
         let f_ty = L.function_type llret_ty llparam_tys in

         (* declare function *)
         let f = L.declare_function name f_ty ctx.ir_module in
         Ctx.bind_val_to_env ctx (LLValue (f, true)) env;

         f
       in

       let define_current_function kind fn_spec =
         let body = Option.get opt_body in

         let param_envs = fn_spec.Env.fn_spec_param_envs in
         let force_inline = fn_spec.Env.fn_spec_force_inline in

         let name = fenv_r.Env.fn_mangled |> Option.get in
         let f = declare_current_function name in

         if force_inline then
           begin
             L.add_function_attr f L.Attribute.Alwaysinline;
             L.add_function_attr f L.Attribute.Nounwind;
             L.set_linkage L.Linkage.Private f;
             ()
           end;

         if is_in_other_module ctx env && not force_inline then
           ()
         else
           begin
             (* entry block *)
             let eib = L.append_block ctx.ir_context "entry" f in
             L.position_at_end eib ctx.ir_builder;

             (* setup parameters *)
             let param_envs = param_envs |> List.enum in
             let raw_ll_params = L.params f |> Array.enum in
             let returns_heavy_obj = is_heavy_object fenv_r.Env.fn_return_type in
             if returns_heavy_obj then
               begin
                 (* set name of receiver *)
                 let opt_agg = Enum.peek raw_ll_params in
                 assert (Option.is_some opt_agg);
                 let agg = Option.get opt_agg in
                 let _ = match kind with
                   | Env.FnKindConstructor (Some venv)
                     | Env.FnKindCopyConstructor (Some venv)
                     | Env.FnKindMoveConstructor (Some venv)
                     | Env.FnKindDefaultConstructor (Some venv)
                     | Env.FnKindDestructor (Some venv) ->
                      let venv_r = Env.VariableOp.get_record venv in
                      let var_name = venv_r.Env.var_name in
                      L.set_value_name var_name agg;
                      Ctx.bind_val_to_env ctx (LLValue (agg, true)) venv
                   | _ ->
                      L.set_value_name agg_recever_name agg;
                 in
                 (* remove the implicit parameter from ENUM *)
                 Enum.drop 1 raw_ll_params;
               end;

             (* adjust type specialized by params to normal type forms *)
             let adjust_param_type (ty, llval) =
               let should_param_be_address = is_address_representation ty in
               let actual_param_rep = is_address_representation_param ty in
               match (should_param_be_address, actual_param_rep) with
               | (true, false) ->
                  let llty = lltype_of_typeinfo ty ctx in
                  let v = L.build_alloca llty "" ctx.ir_builder in
                  let _ = L.build_store llval v ctx.ir_builder in
                  (v, should_param_be_address)
               | (true, true)
                 | (false, false) -> (llval, should_param_be_address)
               | _ -> failwith "[ICE]"
             in
             let ll_params =
               let param_types =
                 fenv_r.Env.fn_param_kinds
                 |> List.map typeinfo_of_paramkind
                 |> List.enum
               in
               Enum.combine (param_types, raw_ll_params)
               |> Enum.map adjust_param_type
             in
             let declare_param_var optenv (llvar, is_addr) =
               match optenv with
               | Some env ->
                  begin
                    let venv = Env.VariableOp.get_record env in
                    let var_name = venv.Env.var_name in
                    L.set_value_name var_name llvar;
                    Ctx.bind_val_to_env ctx (LLValue (llvar, is_addr)) env
                  end
               | None -> ()
             in
             Enum.iter2 declare_param_var param_envs ll_params;

             (* entry block *)
             L.position_at_end eib ctx.ir_builder;

             (**)
             let _ = generate_code body ctx in

             debug_dump_value f;
             Debug.printf "generated genric function(%b): %s\n" env.Env.closed name;

             Llvm_analysis.assert_valid_function f
           end
       in

       match fenv_r.Env.fn_detail with
       | Env.FnRecordNormal (def, kind, fenv_spec) ->
          begin
            define_current_function kind fenv_spec;
            void_val
          end

       | Env.FnRecordImplicit (def, kind, fenv_spec) ->
          begin
            match def with
            (* implicit & trivial *)
            | Env.FnDefDefaulted true ->
               begin
                 assert(List.length fenv_spec.Env.fn_spec_param_envs = 0);
                 let r_value = match kind with
                   | Env.FnKindDefaultConstructor None
                   | Env.FnKindCopyConstructor None
                   | Env.FnKindMoveConstructor None
                   | Env.FnKindConstructor None -> TrivialAction
                   | _ -> failwith "[ICE]"
                 in
                 Ctx.bind_val_to_env ctx r_value env;
                 void_val
               end
            (* implicit & non-trivial *)
            | Env.FnDefDefaulted false ->
               begin
                 define_current_function kind fenv_spec;
                 void_val
               end
            | _ -> failwith "[ICE] define implicit function"
          end

       | Env.FnRecordExternal (def, kind, extern_fname) ->
          begin
            let _ = declare_current_function extern_fname in
            void_val
          end

       | Env.FnRecordBuiltin (def, kind, name) ->
          begin
            (* DO NOTHING *)
            void_val
          end

       | _ -> failwith "[ICE]"
     end

  | TAst.ClassDefStmt (
        name, _, body, opt_attr, Some env
      ) ->
     if Ctx.is_env_defined ctx env then void_val else
     begin
       Ctx.mark_env_as_defined ctx env;

       let cenv_r = Env.ClassOp.get_record env in

       let member_lltypes =
         (* define member variables *)
         let get_member_type index env =
           Ctx.bind_val_to_env ctx (ElemIndex index) env;

           let r = Env.VariableOp.get_record env in
           let llty = lltype_of_typeinfo r.Env.var_type ctx in
           llty
         in
         List.mapi get_member_type cenv_r.Env.cls_member_vars
       in

       let struct_ty =
         L.named_struct_type ctx.Ctx.ir_context
                             (Option.get cenv_r.Env.cls_mangled)
       in
       L.struct_set_body struct_ty (Array.of_list member_lltypes)
                         false (* not packed *);

       Ctx.bind_val_to_env ctx (LLType struct_ty) env;

       debug_dump_type struct_ty;

       (* body *)
       let _ = generate_code body ctx in
       void_val
     end

  | TAst.ExternClassDefStmt (
        name, _, extern_cname, _, _, Some env
      ) ->
     if Ctx.is_env_defined ctx env then void_val else
     begin
       Ctx.mark_env_as_defined ctx env;

       let b_value = try Ctx.find_val_by_name ctx extern_cname with
                     | Not_found ->
                        failwith (Printf.sprintf "[ICE] builtin class \"%s\" is not found"
                                                 extern_cname)
       in
       let ty = match b_value with
         | LLType ty -> ty
         | LLTypeGen f ->
            let cenv_r = Env.ClassOp.get_record env in
            f cenv_r.Env.cls_template_vals
         | _ -> failwith ""
       in
       Ctx.bind_val_to_env ctx (LLType ty) env;
       void_val
     end

  | TAst.VariableDefStmt (_, TAst.VarInit (var_init), Some env) ->
     if Ctx.is_env_defined ctx env then void_val else
     begin
       let venv = Env.VariableOp.get_record env in
       let var_type = venv.Env.var_type in

       let (_, _, (_, opt_init_expr)) = var_init in
       let init_expr = Option.get opt_init_expr in

       let (llval, expr_ty, is_addr) = generate_code init_expr ctx in

       Debug.printf "<><><>\nDefine variable: %s / is_addr: %b\n<><><>\n"
                    venv.Env.var_name
                    is_addr;
       Type.debug_print var_type;

       L.set_value_name venv.Env.var_name llval;
       Ctx.bind_val_to_env ctx (LLValue (llval, is_addr)) env;

       Ctx.mark_env_as_defined ctx env;
       void_val
     end

  | TAst.MemberVariableDefStmt _ -> void_val
  | TAst.EmptyStmt -> void_val

  | TAst.GenericCallExpr (storage_ref, args, Some caller_env, Some env) ->
     begin
       let f_er = Env.FunctionOp.get_record env in
       let {
         Env.fn_name = fn_name;
         Env.fn_detail = detail;
         Env.fn_param_kinds = param_kinds;
         Env.fn_return_type = ret_ty;
       } = f_er in

       let analyze_func_and_eval_args kind =
         let (param_tys, evaled_args) = normalize_params_and_args param_kinds args in
         eval_args_for_func kind storage_ref param_tys ret_ty evaled_args caller_env ctx
       in

       let call_llfunc kind f =
         let (_, llargs, _, returns_addr) = analyze_func_and_eval_args kind in
         let returns_heavy_obj = is_heavy_object ret_ty in
         match returns_heavy_obj with
         | false ->
            let llval = L.build_call f llargs "" ctx.ir_builder in
            (llval, returns_addr)
         | true ->
            let _ = L.build_call f llargs "" ctx.ir_builder in
            assert (Array.length llargs > 0);
            let llval = llargs.(0) in
            (llval, returns_addr)
       in

       let fn_s_name = Id_string.to_string fn_name in
       let (llval, returns_addr) = match detail with
         (* normal function *)
         | Env.FnRecordNormal (_, kind, _) ->
            begin
              Debug.printf "gen value: debug / function normal %s\n" fn_s_name;

              let r_value = force_target_generation ctx env in
              match r_value with
              | LLValue (f, true) -> call_llfunc kind f
              | _ -> failwith "[ICE] unexpected value"
            end

         | Env.FnRecordImplicit (def, kind, _) ->
            begin
              let open Codegen_llvm_intrinsics in
              Debug.printf "gen value: debug / function implicit %s\n" fn_s_name;

              let r_value = force_target_generation ctx env in
              match r_value with
              (* trivial function *)
              | TrivialAction ->
                 begin
                   let (new_obj, is_addr) = match storage_ref with
                     | TAst.StoStack _
                     | TAst.StoAgg _
                     | TAst.StoArrayElem _
                     | TAst.StoMemberVar _ ->
                        let (v, _, is_addr) = setup_storage storage_ref caller_env ctx in
                        (v, is_addr)

                     | _ ->
                        TAst.debug_print_storage storage_ref;
                        failwith "[ICE]"
                   in

                   match kind with
                   | Env.FnKindDefaultConstructor None ->
                      begin
                        (* TODO: zero-fill *)
                        (new_obj, is_addr)
                      end
                   | Env.FnKindCopyConstructor None
                   | Env.FnKindMoveConstructor None ->
                      begin
                        assert (List.length args = 1);
                        let rhs = List.hd args in
                        let (llrhs, rhs_ty, is_ptr) = generate_code rhs ctx in
                        assert (is_ptr);
                        let sl = Type.size_of rhs_ty in
                        let al = Type.align_of rhs_ty in
                        let _ = ctx.intrinsics.memcpy_i32 new_obj llrhs
                                                          sl al false ctx.ir_builder
                        in
                        (new_obj, is_addr)
                      end
                   | _ -> failwith "[ICE]"
                 end

              (* non-trivial function *)
              | LLValue (f, true) -> call_llfunc kind f

              | _ -> failwith "[ICE] unexpected"
            end

         (* external function *)
         | Env.FnRecordExternal (def, kind, extern_name) ->
            begin
              Debug.printf "CALL FUNC / extern  %s = \"%s\"\n"
                            fn_s_name extern_name;
              let (_, llargs, _, returns_addr) =
                analyze_func_and_eval_args kind
              in
              let (extern_f, is_addr) =
                find_llval_by_env_with_force_generation ctx env
              in
              assert (is_addr);
              let llval = L.build_call extern_f llargs "" ctx.ir_builder in
              (llval, returns_addr)
            end

         | Env.FnRecordBuiltin (def, kind, extern_name) ->
            begin
              Debug.printf "CALL FUNC / builtin %s = \"%s\"\n"
                           fn_s_name extern_name;
              TAst.debug_print_storage (storage_ref);
              let (param_tys, llargs, is_addrs, returns_addr) =
                analyze_func_and_eval_args kind
              in
              Debug.printf "== Args\n";
              Array.iter debug_dump_value llargs;
              Debug.printf "== %b\n" returns_addr;

              let v_record = Ctx.find_val_by_name ctx extern_name in
              let builtin_gen_f = match v_record with
                | BuiltinFunc f -> f
                | BuiltinFuncGen f ->
                   let fenv_r = Env.FunctionOp.get_record env in
                   f fenv_r.Env.fn_template_vals
                | _ -> failwith "[ICE]"
              in

              let param_ty_and_addrs = List.combine param_tys is_addrs in
              let ret_ty_and_addrs = (ret_ty, returns_addr) in
              let llval = builtin_gen_f param_ty_and_addrs ret_ty_and_addrs llargs ctx in
              (llval, returns_addr)
            end

         | Env.FnUndef -> failwith "[ICE] codegen: undefined function record"
       in
       (llval, ret_ty, returns_addr)
     end

  | TAst.NestedExpr (lhs_node, _, rhs_ty, Some rhs_env) ->
     begin
       let (ll_lhs, _, is_addr) = generate_code lhs_node ctx in
       assert (is_addr);

       let v_record = try Ctx.find_val_by_env ctx rhs_env with
                      | Not_found -> failwith "[ICE] member env is not found"
       in
       let index = match v_record with
         | ElemIndex i -> i
         | _ -> failwith "[ICE]"
       in
       let llelem = L.build_struct_gep ll_lhs index "" ctx.ir_builder in
       (llelem, rhs_ty, true)
     end

  | TAst.FinalyzeExpr (opt_act_node, final_exprs) ->
     let (ll_lhs, ty, is_addr) = match opt_act_node with
       | Some act_node -> generate_code act_node ctx
       | None -> (void_v, void_ty, false)
     in
     List.iter (fun n -> let _ = generate_code n ctx in ()) final_exprs;
     (ll_lhs, ty, is_addr)

  | TAst.SetCacheExpr (id, node) ->
     let values = generate_code node ctx in
     Ctx.bind_values_to_cache_id ctx values id;
     values

  | TAst.GetCacheExpr id ->
     Ctx.find_values_by_cache_id ctx id

  | TAst.BoolLit (v, lit_ty) ->
     begin
       let llty = L.i1_type ctx.ir_context in
       let llval = L.const_int llty (if v then 1 else 0) in
       let (llval, is_addr) = adjust_primitive_value storage llty llval ctx in
       (llval, lit_ty, false)
     end

  | TAst.IntLit (v, bits, signed, lit_ty) ->
     begin
       let llty = match bits with
         | 8 -> L.i8_type ctx.ir_context
         | 32 -> L.i32_type ctx.ir_context
         | _ -> failwith "[ICE]"
       in
       let llval = L.const_int llty v in
       let (llval, is_addr) = adjust_primitive_value storage llty llval ctx in
       (llval, lit_ty, is_addr)
     end

  | TAst.StringLit (str, lit_ty) ->
     begin
       let llty = L.pointer_type (L.i8_type ctx.ir_context) in
       let llval = L.build_global_stringptr str "" ctx.ir_builder in
       let (llval, is_addr) = adjust_primitive_value storage llty llval ctx in
       (llval, lit_ty, false)
     end

  | TAst.ArrayLit (elems, static_constructable, arr_ty) ->
     begin
       let arr_llty = lltype_of_typeinfo arr_ty ctx in
       let ll_array_sto = L.build_alloca arr_llty "" ctx.ir_builder in

       if static_constructable then
         begin
           let (ll_elems, tys, _) =
             let res = elems |> List.map (fun n -> generate_code n ctx) in
             List.fold_right (fun (v, t, p) (vs, ts, ps) -> (v::vs, t::ts, p::ps))
                             res
                             ([], [], [])
           in
           let lit_ty = List.hd tys in
           let llty = lltype_of_typeinfo lit_ty ctx in
           let lit_ptr_ty = L.pointer_type llty in
           let ll_array = L.const_array llty (Array.of_list ll_elems) in
           let ll_array = L.define_global "" ll_array ctx.ir_module in
           L.set_linkage L.Linkage.Private ll_array;
           L.set_unnamed_addr true ll_array;
           let llsrc = L.build_bitcast ll_array lit_ptr_ty "" ctx.ir_builder in
           let lltrg = L.build_bitcast ll_array_sto lit_ptr_ty "" ctx.ir_builder in
           let size_of = Type.size_of arr_ty in
           let align_of = Type.align_of arr_ty in
           let open Codegen_llvm_intrinsics in
           let _ =
             ctx.intrinsics.memcpy_i32 lltrg llsrc
                                       size_of align_of false ctx.ir_builder
           in
           (ll_array_sto, arr_ty, true)
         end
       else
         begin
           Ctx.push_array_storage ctx (LLValue (ll_array_sto, true));
           let _ = elems |> List.map (fun n -> generate_code n ctx) in
           let _ = Ctx.pop_array_storage ctx in
           (ll_array_sto, arr_ty, true)
         end
     end

  | TAst.GenericId (name, lt_args, Some rel_env) ->
     begin
       let { Env.er = er; _ } = rel_env in
       match er with
       | Env.Function (_, r) ->
          begin
            assert(List.length lt_args = 0);
            (* *)
            Env.debug_print rel_env;
            failwith @@ "[ICE] TAst.Id: function "
                        ^ (Id_string.to_string name)
                        ^ " // "
                        ^ (Id_string.to_string r.Env.fn_name)
          end

       | Env.Variable (r) ->
          begin
            assert(List.length lt_args = 0);
            Debug.printf "variable %s / type => %s\n"
                         (Id_string.to_string name)
                         (Type.to_string r.Env.var_type);
            let var_llr = try Ctx.find_val_by_env ctx rel_env with
                          | Not_found ->
                             begin
                               try Ctx.find_metaval_by_env ctx rel_env with
                               | Not_found -> failwith "[ICE] variable env is not found"
                             end
            in
            let (llval, is_addr) = match var_llr with
              | LLValue v -> v
              | _ -> failwith "[ICE]"
            in
            let ty = r.Env.var_type in
            (llval, ty, is_addr)
          end

       | Env.Class (_, r) ->
          begin
            let ty_attr_val_default = {
              Type_attr.ta_ref_val = Type_attr.Val;
              Type_attr.ta_mut = Type_attr.Const;
            } in
            (* generics *)
            assert (List.length r.Env.cls_generics_vals >= List.length lt_args);
            let generics_args =
              let rec f pl al acc =
                match (pl, al) with
                | ([], []) -> acc
                | ([], _) -> failwith ""
                | (_::px, []) -> f px [] (Lifetime.LtUndef :: acc)  (* TODO: check undef *)
                | (p::px, a::ax) -> f px ax (a :: acc)
              in
              f r.Env.cls_generics_vals lt_args [] |> List.rev
            in

            (* type *)
            let ty =
              Type.Generator.generate_type ctx.type_sets.Type_sets.ts_type_gen
                                           (Type_info.UniqueTy rel_env)
                                           r.Env.cls_template_vals
                                           generics_args
                                           ty_attr_val_default
            in
            let itype_id = Option.get ty.Type_info.ti_id in
            Debug.printf "#### LLVM: eval class: type_id = %s / %s\n"
                         (Int64.to_string itype_id)
                         (Type.to_string ty);

            (* return the internal typeid as a type *)
            let llval = L.const_of_int64 (L.i64_type ctx.ir_context)
                                         itype_id
                                         Type_info.is_type_id_signed
            in
            let ty = ctx.type_sets.Type_sets.ts_type_type in
            (llval, ty, false)
          end

       (* regards all values are NOT references *)
       | Env.MetaVariable (uni_id) ->
          begin
            assert(List.length lt_args = 0);
            let uni_map = ctx.uni_map in

            Debug.printf "LLVM codegen Env.MetaVariable = %d\n" uni_id;
            let (_, c) = Unification.search_value_until_terminal uni_map uni_id in
            match c with
            | Unification.Val v -> ctfe_val_to_llval v ctx
            | _ -> raise @@ Ctfe_exn.Meta_var_un_evaluatable uni_id
          end

       | _ ->
          begin
            Env.debug_print rel_env;
            failwith @@ "codegen; id " ^ (Id_string.to_string name)
          end
     end

  | TAst.ScopeExpr (block) ->
     begin
       generate_code block ctx
     end

  (* TODO: fix is_addr *)
  | TAst.IfExpr (cond_expr, then_expr, opt_else_expr, if_ty) ->
     begin
       let (llcond, _, is_cond_addr) = generate_code cond_expr ctx in
       let llcond = if is_cond_addr then
                      L.build_load llcond "" ctx.ir_builder
                    else
                      llcond
       in

       let ip =  L.insertion_block ctx.Ctx.ir_builder in
       let tip = L.insert_block ctx.ir_context "then" ip in (* then *)
       let eip = L.insert_block ctx.ir_context "else" ip in (* else *)
       let fip = L.insert_block ctx.ir_context "term" ip in (* final *)
       L.move_block_after ip tip;
       L.move_block_after tip eip;
       L.move_block_after eip fip;

       let _ = match opt_else_expr with
         | Some _ ->
            (* true -> true block, false -> else block *)
            L.build_cond_br llcond tip eip ctx.ir_builder
         | None ->
            (* true -> true block, false -> final block *)
            L.build_cond_br llcond tip fip ctx.ir_builder
       in

       (* then node *)
       L.position_at_end tip ctx.Ctx.ir_builder;
       let (then_llval, then_ty, is_then_addr) = generate_code then_expr ctx in
       let then_llval = adjust_llval_form if_ty then_ty then_llval ctx in
       let then_llval =
         adjust_addr_val then_llval then_ty is_then_addr ctx
       in
       let then_branch = if not (L.is_terminator then_llval) then
                           ignore @@ L.build_br fip ctx.ir_builder;
                         let cip = L.insertion_block ctx.ir_builder in
                         (then_llval, cip)
       in

       let brs = match opt_else_expr with
         | Some else_expr ->
            begin
              L.position_at_end eip ctx.Ctx.ir_builder;
              let (else_llval, else_ty, is_else_addr) = generate_code else_expr ctx in
              let else_llval = adjust_llval_form if_ty else_ty else_llval ctx in
              let else_llval =
                adjust_addr_val else_llval else_ty is_else_addr ctx
              in
              if not (L.is_terminator else_llval) then
                ignore @@ L.build_br fip ctx.ir_builder;
              let cip = L.insertion_block ctx.ir_builder in
              [then_branch; (else_llval, cip)]
            end
         | None ->
            begin
              L.remove_block eip;
              [then_branch]
            end
       in

       L.position_at_end fip ctx.Ctx.ir_builder;
       if Type.has_same_class ctx.type_sets.Type_sets.ts_void_type then_ty then
         void_val
       else
         let llret = L.build_phi brs "" ctx.ir_builder in
         (llret, if_ty, is_address_representation if_ty)
     end

  (* FIX: is_addr *)
  | TAst.ForExpr (opt_decl, opt_cond, opt_step, body) ->
     begin
       let ip =  L.insertion_block ctx.Ctx.ir_builder in
       let bip = L.insert_block ctx.ir_context "loop_begin" ip in
       let sip = L.insert_block ctx.ir_context "loop_step" ip in
       let eip = L.insert_block ctx.ir_context "loop_end" ip in
       L.move_block_after ip bip;
       L.move_block_after bip sip;
       L.move_block_after sip eip;

       let _ = match opt_decl with
         | Some decl -> ignore @@ generate_code decl ctx
         | None -> ()
       in

       let _ = L.build_br bip ctx.ir_builder in
       L.position_at_end bip ctx.Ctx.ir_builder;

       let _ = match opt_cond with
         | Some cond ->
            begin
              let (llcond, _, _) = generate_code cond ctx in
              ignore @@ L.build_cond_br llcond sip eip ctx.ir_builder;
              L.position_at_end sip ctx.Ctx.ir_builder;
            end
         | None -> L.remove_block sip;
       in

       ignore @@ generate_code body ctx;

       let _ = match opt_step with
         | Some step ->
            begin
              ignore @@ generate_code step ctx
            end
         | None -> ();
       in

       ignore @@ L.build_br bip ctx.ir_builder;
       L.position_at_end eip ctx.Ctx.ir_builder;

       void_val
     end

  | TAst.CtxNode ty ->
     begin
       let itype_id = Option.get ty.Type_info.ti_id in
       Debug.printf "##### type_id = %s\n" (Int64.to_string itype_id);

       (* return the internal typeid as a type *)
       let llval = L.const_of_int64 (L.i64_type ctx.ir_context)
                                    itype_id
                                    Type_info.is_type_id_signed
       in
       let ty = ctx.type_sets.Type_sets.ts_type_type in
       (llval, ty, false)
     end

  | TAst.StorageWrapperExpr (sto, e) ->
     Debug.printf "STORAGE CHANGED\n";
     TAst.debug_print_storage (!sto);

     generate_code ~storage:(Some !sto) e ctx

  | _ -> failwith "cannot generate : node"


and ctfe_val_to_llval ctfe_val ctx =
  let open Ctx in
  let tsets = ctx.type_sets in
  match ctfe_val with
  | Ctfe_value.Type (ty) ->
     begin
       let {
         Type_info.ti_id = opt_tid;
         Type_info.ti_sort = ty_sort;
       } = ty in
       match ty_sort with
       | Type_info.UniqueTy _
       | Type_info.NotDetermined _ ->
          begin
            match opt_tid with
            | Some tid ->
               (* return the internal typeid as a type *)
               let llval =
                 L.const_of_int64 (L.i64_type ctx.ir_context)
                                  tid
                                  Type_info.is_type_id_signed
               in
               let ty = tsets.Type_sets.ts_type_type in
               (llval, ty, false)
            | None -> failwith "[ICE] codegen_llvm : has no type_id"
          end
       | _-> failwith "[ICE] codegen_llvm : type"
     end

  | Ctfe_value.Bool b ->
     let llval =
       L.const_of_int64 (L.i1_type ctx.ir_context)
                        (Int64.of_int (Bool.to_int b))
                        false (* not signed *)
     in
     let ty = !(tsets.Type_sets.ts_bool_type_holder) in
     (llval, ty, false)

  | Ctfe_value.Int32 i32 ->
     let llval =
       L.const_of_int64 (L.i32_type ctx.ir_context)
                        (Int32.to_int64 i32)
                        true (* signed *)
     in
     let ty = !(tsets.Type_sets.ts_int32_type_holder) in
     (llval, ty, false)

  | Ctfe_value.Uint32 i32 ->
     let llval =
       L.const_of_int64 (L.i32_type ctx.ir_context)
                        (Uint32.to_int64 i32)
                        false (* unsigned *)
     in
     let ty = !(tsets.Type_sets.ts_int32_type_holder) in
     (llval, ty, false)

  | Ctfe_value.Undef ud_uni_id ->
     raise @@ Ctfe_exn.Meta_var_un_evaluatable ud_uni_id

  | _ -> failwith "[ICE]"

and is_in_other_module ctx env =
  let open Ctx in
  match ctx.target_module_id with
  | None -> false
  | Some target_module_id ->
     let module_env_id = match Env.get_module_env_id env with
       | Some e -> e
       | None -> failwith "[ICE]"
     in
     if target_module_id = module_env_id then
       false
     else
       not env.Env.is_instantiated

and generate_code_by_interrupt node ctx =
  let opt_ip = match L.insertion_block ctx.Ctx.ir_builder with
    | exception Not_found -> None
    | x -> Some x
  in

  let _ = generate_code node ctx in

  opt_ip |> Option.may (fun ip -> L.position_at_end ip ctx.Ctx.ir_builder)


and force_target_generation ctx env =
  try Ctx.find_val_by_env ctx env with
  | Not_found ->
     begin
       let { Env.rel_node = rel_node; _ } = env in
       let node = match rel_node with
         | Some v -> v
         | None ->
            begin
              Env.debug_print env;
              failwith "[ICE] force_target_generation: there is no rel node"
            end
       in
       generate_code_by_interrupt node ctx;

       (* retry *)
       try Ctx.find_val_by_env ctx env with
       | Not_found ->
          begin
            Env.debug_print env;
            failwith "[ICE] force_target_generation: couldn't find target"
          end
     end
  | _ -> failwith "yo"

and find_llval_by_env_with_force_generation ctx env =
  let v_record = force_target_generation ctx env in
  match v_record with
  | LLValue v -> v
  | _ -> failwith "[ICE] / find_llval_by_env_with_force_generation"

and find_lltype_by_env_with_force_generation ctx env =
  let v_record = force_target_generation ctx env in
  match v_record with
  | LLType t -> t
  | _ -> failwith "[ICE] / find_lltype_by_env_with_force_generation"


and register_metaval value is_addr env ctx =
  let (llval, _, _) = ctfe_val_to_llval value ctx in
  let cv = LLValue (llval, is_addr) in
  Ctx.bind_metaval_to_env ctx cv env


and lltype_of_typeinfo ty ctx =
  let open Ctx in
  let cenv = Type.as_unique ty in
  let ll_ty = find_lltype_by_env_with_force_generation ctx cenv in
  ll_ty

and lltype_of_typeinfo_param ty ctx =
  let ll_ty = lltype_of_typeinfo ty ctx in
  if is_address_representation_param ty then
    L.pointer_type ll_ty
  else
    ll_ty

and lltype_of_typeinfo_ret ty ctx =
  let ll_ty = lltype_of_typeinfo ty ctx in
  if is_address_representation ty then
    L.pointer_type ll_ty
  else
    ll_ty

and is_primitive ty =
  let cenv = Type.as_unique ty in
  let cr = Env.ClassOp.get_record cenv in
  cr.Env.cls_traits.Env.cls_traits_is_primitive

and is_always_value ty =
  let cenv = Type.as_unique ty in
  let cr = Env.ClassOp.get_record cenv in
  cr.Env.cls_traits.Env.cls_traits_is_always_value

and is_heavy_object ty =
  let {
    Type_attr.ta_ref_val = rv;
    Type_attr.ta_mut = mut;
  } = ty.Type_info.ti_attr in
  match rv with
  | Type_attr.Ref _ -> false
  | Type_attr.Val -> is_address_representation ty
  | _ -> failwith "[ICE] Unexpected : rv"

(**)
and is_address_representation ty =
  let {
    Type_attr.ta_ref_val = rv;
    Type_attr.ta_mut = mut;
  } = ty.Type_info.ti_attr in
  match rv with
  | Type_attr.Ref _ -> not (is_always_value ty)
  | Type_attr.Val ->
     begin
       match mut with
       | Type_attr.Mutable -> true
       | Type_attr.Const
       | Type_attr.Immutable ->
          begin
            (* if type is NOT primitive, it will be represented by address *)
            not (is_primitive ty)
          end
       | _ -> failwith "[ICE] Unexpected : mut"
     end
  | _ -> failwith "[ICE] Unexpected : rv"

(* address representation of function interface *)
and is_address_representation_param ty =
  let {
    Type_attr.ta_ref_val = rv;
  } = ty.Type_info.ti_attr in
  match rv with
  | Type_attr.Val when is_primitive ty -> false
  | _ -> is_address_representation ty

and adjust_llval_form' trg_ty trg_chkf src_ty src_chkf skip_alloca llval ctx =
  let open Ctx in
  match (trg_chkf trg_ty, src_chkf src_ty) with
  | (true, true)
  | (false, false) -> llval
  | (true, false) ->
     if skip_alloca then
       llval
     else
       let llty = lltype_of_typeinfo trg_ty ctx in
       let v = L.build_alloca llty "" ctx.ir_builder in
       let _ = L.build_store llval v ctx.ir_builder in
       v
  | (false, true) ->
     if skip_alloca then
       llval
     else
       (debug_dump_value llval;
       L.build_load llval "" ctx.ir_builder)

and adjust_llval_form trg_ty src_ty llval ctx =
  Debug.printf "is_pointer rep?  trg: %b(%s), src: %b(%s)\n"
               (is_address_representation trg_ty)
               (Type.to_string trg_ty)
               (is_address_representation src_ty)
               (Type.to_string src_ty);
  debug_dump_value llval;

  let trg_check_f = is_address_representation in
  let src_check_f = is_address_representation in
  adjust_llval_form' trg_ty trg_check_f src_ty src_check_f false llval ctx

and adjust_arg_llval_form trg_ty src_ty src_is_addr skip_alloca llval ctx =
  if is_primitive trg_ty then
    begin
      Debug.printf "ARG: is_pointer_arg rep?  trg: %b(%s), src: %b, %b(%s)\n"
                   (is_address_representation_param trg_ty)
                   (Type.to_string trg_ty)
                   (is_address_representation src_ty)
                   src_is_addr
                   (Type.to_string src_ty);
      debug_dump_value llval;

      let trg_check_f = is_address_representation_param in
      let src_check_f = is_address_representation in (ignore @@ src_check_f);
      let src_check_f = fun _ -> src_is_addr in
      adjust_llval_form' trg_ty trg_check_f src_ty src_check_f skip_alloca llval ctx
    end
  else
    adjust_llval_form trg_ty src_ty llval ctx

and adjust_addr_val v ty is_addr ctx =
  if is_address_representation ty then
    v
  else
    if is_addr then
      L.build_load v "" ctx.Ctx.ir_builder
    else
      v

(* allocate storage for primitve i*)
and adjust_primitive_value storage llty llval ctx =
  match storage with
  | Some (TAst.StoStack _) ->
     let v = L.build_alloca llty "" ctx.Ctx.ir_builder in
     let _ = L.build_store llval v  ctx.Ctx.ir_builder in
     (v, true)
  | Some TAst.StoImm
  | None -> (llval, false)
  | _ -> failwith "[ICE]"


and paramkinds_to_llparams params ret_ty ctx =
  let returns_heavy_obj = is_heavy_object ret_ty in
  let f (is_vargs, rev_tys) tp =
    match tp with
    | Env.FnParamKindType ty -> (is_vargs, ty :: rev_tys)
  in
  let (is_vargs, rev_tys) =
    List.fold_left f (false, []) params
  in
  let param_types = rev_tys |> List.rev in
  let llparams =
    (if returns_heavy_obj then [ret_ty] else []) @ param_types
    |> List.map (fun t -> lltype_of_typeinfo_param t ctx)
    |> Array.of_list
  in
  (is_vargs, llparams)

(* move to elsewhere *)
and typeinfo_of_paramkind pk =
  match pk with
  | Env.FnParamKindType ty -> ty

and normalize_params_and_args params_info args =
  adjust_param_types' params_info args []
  |> List.rev |> List.split

and adjust_param_types' params_info args acc =
  match (params_info, args) with
  | (param_info :: px, arg :: ax) ->
     begin
       match param_info with
       | Env.FnParamKindType ty ->
          adjust_param_types' px ax ((ty, arg) :: acc)
     end
  | (_, []) -> acc
  | ([], _) -> failwith "[ICE]"

and setup_storage sto caller_env ctx =
  let open Ctx in
  match sto with
  | TAst.StoStack (ty) ->
     begin
       Debug.printf "setup_storage: StoStack ty=%s\n"
                    (Type.to_string ty);
       let llty = lltype_of_typeinfo ty ctx in
       let v = L.build_alloca llty "" ctx.ir_builder in
       (v, ty, true)
     end

  | TAst.StoAgg (ty) ->
     begin
       Debug.printf "setup_storage: StoAgg ty=%s\n"
                    (Type.to_string ty);
       let ctx_env = caller_env.Env.context_env in
       let (ll_fval, is_f_addr) =
         find_llval_by_env_with_force_generation ctx ctx_env
       in
       assert (is_f_addr);
       let agg = L.param ll_fval 0 in
       assert (L.value_name agg = agg_recever_name);
       (agg, ty, true)
     end

  | TAst.StoArrayElem (ty, index) ->
     Debug.printf "setup_storage: StoArrayElem ty=%s\n"
                  (Type.to_string ty);
     let array_sto = match Ctx.current_array_storage ctx with
       | LLValue (v, true) -> v
       | _ -> failwith "[ICE]"
     in
     let zero = L.const_int (L.i32_type ctx.ir_context) 0 in
     let llindex = L.const_int (L.i32_type ctx.ir_context) index in
     let array_elem_ptr =
       L.build_in_bounds_gep array_sto
                             [|zero; llindex|]
                             ""
                             ctx.Ctx.ir_builder
     in
     (array_elem_ptr, ty, true)

  | TAst.StoArrayElemFromThis (ty, Some this_env, index) ->
     Debug.printf "setup_storage: StoArrayElemFromThis ty=%s\n"
                  (Type.to_string ty);
     let (array_sto, is_f_addr) =
       find_llval_by_env_with_force_generation ctx this_env
     in
     assert (is_f_addr);
     let zero = L.const_int (L.i32_type ctx.ir_context) 0 in
     let llindex = L.const_int (L.i32_type ctx.ir_context) index in
     let array_elem_ptr =
       L.build_in_bounds_gep array_sto
                             [|zero; llindex|]
                             ""
                             ctx.Ctx.ir_builder
     in
     (array_elem_ptr, ty, true)

  | TAst.StoMemberVar (ty, Some venv, Some parent_fenv) ->
     Debug.printf "setup_storage: StoMemberVar ty=%s\n"
                  (Type.to_string ty);
     let (reciever_llval, is_addr) =
       match Env.FunctionOp.get_kind parent_fenv with
       | Env.FnKindConstructor (Some rvenv)
       | Env.FnKindCopyConstructor (Some rvenv)
       | Env.FnKindMoveConstructor (Some rvenv)
       | Env.FnKindDefaultConstructor (Some rvenv)
       | Env.FnKindDestructor (Some rvenv) ->
          find_llval_by_env_with_force_generation ctx rvenv
       | _ -> failwith "[ICE] no reciever"
     in
     assert (is_addr);
     let member_index = match Ctx.find_val_by_env ctx venv with
       | ElemIndex idx -> idx
       | _ -> failwith "[ICE] a member variable is not found"
     in
     let elem_llval =
       debug_dump_value reciever_llval;
       Debug.printf "index = %d\n" member_index;

       L.build_struct_gep reciever_llval member_index "" ctx.ir_builder
     in
     (elem_llval, ty, true)

  | _ -> failwith "[ICE] cannot setup storage"

and eval_args_for_func kind sto param_types ret_ty args caller_env ctx =
  (* normal arguments *)
  let (llvals, arg_tys, is_addrs) =
    let res = args |> List.map (fun n -> generate_code n ctx) in
    List.fold_right (fun (v, t, p) (vs, ts, ps) -> (v::vs, t::ts, p::ps))
                    res
                    ([], [], [])
  in
  (* special arguments *)
  let (llvals, arg_tys, is_addrs, param_types, returns_addr, skip_alloca) =
    match sto with
    | TAst.StoStack _
    | TAst.StoAgg _
    | TAst.StoArrayElem _
    | TAst.StoArrayElemFromThis _
    | TAst.StoMemberVar _ ->
       let (v, ty, is_addr) = setup_storage sto caller_env ctx in
       let sa = is_primitive ty in
       (v::llvals, ty::arg_tys, is_addr::is_addrs, ty::param_types, true, sa)

    | TAst.StoImm ->
       let (returns_addr, sa) =
         match kind with
         | Env.FnKindFree ->
            (is_address_representation ret_ty, false)

         | _ ->
            let (returns_addr, skip_alloca) = match (is_addrs, arg_tys) with
              (* Case for constructors of primitives *)
              | ([], []) -> (false, false)
              (* Case for normal constructors
               * An address flag of first arg(hp) means
               *   "is the return value pointer?"
               * And if the first type of arguments is a primitive,
               *   do not allocate storage for optimization
               *)
              | (hp::_, aty::_) -> (hp, is_primitive aty)
              | _ -> failwith "[ICE]"
            in
            (returns_addr, skip_alloca)
       in
       (llvals, arg_tys, is_addrs, param_types, returns_addr, sa)

    | _ -> failwith (Printf.sprintf "[ICE] special arguments %s"
                                    (TAst.string_of_stirage sto))
  in
  let llargs =
    Debug.printf "conv funcs skip_alloca = %b\n" skip_alloca;
    let rec make param_tys arg_tys is_addrs llvals =
      match (param_tys, arg_tys, is_addrs, llvals) with
      | ([], [], [], []) -> []
      | (pt::pts, at::ats, ia::ias, lv::lvs) ->
         let llarg = adjust_arg_llval_form pt at ia skip_alloca lv ctx in
         llarg::(make pts ats ias lvs)
      | _ -> failwith ""
    in
    make param_types arg_tys is_addrs llvals
  in
  let llargs = llargs |> Array.of_list in
  (param_types, llargs, is_addrs, returns_addr)

let regenerate_module ctx =
  let ir_module = L.create_module ctx.Ctx.ir_context "Rill" in
  ctx.Ctx.ir_module <- ir_module


let inject_builtins ctx =
  let open Ctx in
  let register_builtin_type name record =
    Ctx.bind_val_to_name ctx record name;
    (*Debug.printf "debug / registerd builtin type = \"%s\"\n" name*)
  in
  let register_builtin_func name f =
    Ctx.bind_val_to_name ctx (BuiltinFunc f) name;
    (*Debug.printf "debug / registerd builtin func = \"%s\"\n" name*)
  in
  let register_builtin_template_func name f =
    Ctx.bind_val_to_name ctx (BuiltinFuncGen f) name;
    (*Debug.printf "debug / registerd builtin func = \"%s\"\n" name*)
  in

  (* type is represented as int64 in this context.
   * It donates ID of type in the type generator
   *)
  begin
    let open Builtin_info in
    (*
     * Builtin types
     *)
    register_builtin_type type_type_i.internal_name
                          (LLType (L.i64_type ctx.ir_context));

    register_builtin_type void_type_i.internal_name
                          (LLType (L.void_type ctx.ir_context));
    register_builtin_type bool_type_i.internal_name
                          (LLType (L.i1_type ctx.ir_context));
    register_builtin_type uint8_type_i.internal_name
                          (LLType (L.i8_type ctx.ir_context));
    register_builtin_type int32_type_i.internal_name
                          (LLType (L.i32_type ctx.ir_context));
    register_builtin_type uint32_type_i.internal_name
                          (LLType (L.i32_type ctx.ir_context));

    register_builtin_type raw_ptr_type_i.internal_name
                          (LLTypeGen (
                               fun template_args ->
                               begin
                                 assert (List.length template_args = 1);
                                 let ty_ct_val = List.nth template_args 0 in
                                 let ty_val = match ty_ct_val with
                                   | Ctfe_value.Type ty -> ty
                                   | _ -> failwith "[ICE]"
                                 in
                                 let elem_ty = lltype_of_typeinfo ty_val ctx in
                                 L.pointer_type elem_ty
                               end
                             ));
    register_builtin_type untyped_raw_ptr_type_i.internal_name
                          (LLType (L.pointer_type (L.i8_type ctx.ir_context)));

    register_builtin_type array_type_i.internal_name
                          (LLTypeGen (
                               fun template_args ->
                               begin
                                 assert (List.length template_args = 2);
                                 let ty_ct_val = List.nth template_args 0 in
                                 let len_ct_val = List.nth template_args 1 in
                                 let ty_val = match ty_ct_val with
                                   | Ctfe_value.Type ty -> ty
                                   | _ -> failwith "[ICE]"
                                 in
                                 let len_val = match len_ct_val with
                                   | Ctfe_value.Uint32 i32 -> i32
                                   | _ -> failwith "[ICE]"
                                 in
                                 (* TODO: fix *)
                                 let llty = lltype_of_typeinfo ty_val ctx in
                                 let len = Uint32.to_int len_val in
                                 L.array_type llty len
                               end
                             ));
  end;

  (*
   * Builtin functions
   *)
  let () =
    (* sizeof is onlymeta function *)
    let f _ _ args ctx =
      assert (Array.length args = 1);
      let ty_val = args.(0) in
      let ty = match L.int64_of_const ty_val with
        | Some t_id ->
           Type.Generator.find_type_by_cache_id ctx.type_sets.Type_sets.ts_type_gen t_id
        | _ -> failwith "[ICE] failed to get a ctfed value"
      in
      llval_u32 (Type.size_of ty) ctx
    in
    register_builtin_func "__builtin_sizeof" f
  in

  let () =
    let f template_args _ _ args ctx =
      assert (List.length template_args = 1);
      let ty_val = List.nth template_args 0 in
      let ty = match ty_val with
        | Ctfe_value.Type ty -> ty
        | _ -> failwith "[ICE]"
      in
      let type_s = Type.to_string ty in
      L.build_global_stringptr type_s "" ctx.ir_builder
    in
    register_builtin_template_func "__builtin_stringof" f
  in

  let () =
    let f template_args _ _ args ctx =
      assert (List.length template_args = 1);
      let ty_val = List.nth template_args 0 in
      let ty = match ty_val with
        | Ctfe_value.Type ty -> ty
        | _ -> failwith "[ICE]"
      in
      let llty = lltype_of_typeinfo ty ctx in
      let llptrty = L.pointer_type llty in

      assert (Array.length args = 1);
      L.build_bitcast args.(0) llptrty "" ctx.ir_builder
    in
    register_builtin_template_func "__builtin_unsafe_ptr_cast" f
  in

  let () =
    let f template_args _ _ args ctx =
      assert (List.length template_args = 2);
      let ty_val = List.nth template_args 0 in
      let ty = match ty_val with
        | Ctfe_value.Type ty -> ty
        | _ -> failwith "[ICE]"
      in
      let llty = lltype_of_typeinfo ty ctx in
      let llptrty = L.pointer_type llty in

      assert (Array.length args = 1);

      L.build_pointercast args.(0) llptrty "" ctx.ir_builder
    in
    register_builtin_template_func "__builtin_take_address_from_array" f
  in
  let () =
    let f param_tys_and_addrs _ args ctx =
      let open Codegen_llvm_intrinsics in
      assert (Array.length args = 1);
      assert (List.length param_tys_and_addrs = 1);
      let (arr_ty, _) = List.hd param_tys_and_addrs in

      let to_obj = args.(0) in
      let size_of = Type.size_of arr_ty in
      let align_of = Type.align_of arr_ty in
      let _ =
        ctx.intrinsics.memset_i32 to_obj (Int8.of_int 0)
                                  size_of align_of false ctx.ir_builder
      in
      to_obj
    in
    let open Builtin_info in
    register_builtin_func
      (make_builtin_default_ctor_name array_type_i.internal_name) f
  in
  let () =
    let f param_tys_and_addrs _ args ctx =
      let open Codegen_llvm_intrinsics in
      assert (Array.length args = 2);
      assert (List.length param_tys_and_addrs = 2);
      let (arr_ty, _) = List.hd param_tys_and_addrs in

      let to_obj = args.(0) in
      let from_obj = args.(1) in
      let size_of = Type.size_of arr_ty in
      let align_of = Type.align_of arr_ty in
      let _ =
        ctx.intrinsics.memcpy_i32 to_obj from_obj
                                  size_of align_of false ctx.ir_builder
      in
      to_obj
    in
    let open Builtin_info in
    register_builtin_func
      (make_builtin_copy_ctor_name array_type_i.internal_name) f
  in

  let define_special_members builtin_info init_val_gen =
    let open Builtin_info in

    let normalize_store_value param_tys_and_addrs args =
      assert (List.length param_tys_and_addrs = Array.length args);
      let (_, rhs_is_addr) = List.at param_tys_and_addrs 1 in
      Debug.printf "COPY => %b\n" rhs_is_addr;
      if rhs_is_addr then
        L.build_load args.(1) "" ctx.ir_builder
      else
        args.(1)
    in

    let () = (* default constructor *)
      let f param_tys_and_addrs ret_ty_and_addr args ctx =
        assert (List.length param_tys_and_addrs = Array.length args);
        let v = init_val_gen ret_ty_and_addr in
        match Array.length args with
        | 1 ->
           let _ = L.build_store v args.(0) ctx.ir_builder in args.(0)
        | 0 -> v
        | _ -> failwith ""
      in
      register_builtin_func
        (make_builtin_default_ctor_name builtin_info.internal_name) f
    in
    let () = (* copy constructor *)
      let f param_tys_and_addrs _ args ctx =
        assert (List.length param_tys_and_addrs = Array.length args);
        match Array.length args with
        | 2 ->
           let store_val = normalize_store_value param_tys_and_addrs args in
           let _ = L.build_store store_val args.(0) ctx.ir_builder in
           args.(0)
        | 1 -> args.(0)
        | _ -> failwith ""
      in
      register_builtin_func
        (make_builtin_copy_ctor_name builtin_info.internal_name) f
    in
    let () = (* copy assign *)
      let f param_tys_and_addrs _ args ctx =
        assert (Array.length args = 2);
        let store_val = normalize_store_value param_tys_and_addrs args in
        L.build_store store_val args.(0) ctx.Ctx.ir_builder
      in
      register_builtin_func
        (make_builtin_copy_assign_name builtin_info.internal_name) f
    in
    ()
  in

  (*let _ =
    (* defaulted but not trivial *)
    let f args ctx =
      assert (Array.length args = 2);
      failwith ""
    in
    register_builtin_func "__builtin_array_type_copy_ctor" f
  in*)

  (* for type *)
  let () =
    let () = (* ==(:type, :type): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        let lhs_ty_val = args.(0) in
        let lhs_ty = match L.int64_of_const lhs_ty_val with
          | Some t_id ->
             Type.Generator.find_type_by_cache_id ctx.type_sets.Type_sets.ts_type_gen t_id
          | _ -> failwith "[ICE] failed to get a ctfed value"
        in
        let rhs_ty_val = args.(1) in
        let rhs_ty = match L.int64_of_const rhs_ty_val with
          | Some t_id ->
             Type.Generator.find_type_by_cache_id ctx.type_sets.Type_sets.ts_type_gen t_id
          | _ -> failwith "[ICE] failed to get a ctfed value"
        in
        let _ = lhs_ty in
        let _ = rhs_ty in
        llval_i1 (true) ctx (* TODO: fix *)
      in
      register_builtin_func "__builtin_op_binary_==_type_type" f
    in
    ()
  in

  (* for int8 *)
  let () =
    let open Builtin_info in
    let init _ = L.const_int (L.i8_type ctx.ir_context) 0 in
    define_special_members uint8_type_i init;
    ()
  in

  let () =
    let open Builtin_info in

    let () = (* identity *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        args.(0)
      in
      register_builtin_func "__builtin_identity" f
    in

    let () = (* +(:uint32): uint8 *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        L.build_intcast args.(0) (L.i8_type ctx.ir_context) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_cast_from_uint32_to_uint8" f
    in

    let () = (* +(:uint8): int32 *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        L.build_intcast args.(0) (L.i32_type ctx.ir_context) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_cast_from_uint8_to_int32" f
    in

    let () = (* +(:int32): uint32 *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        (* unsigned, use zext *)
        L.build_zext_or_bitcast args.(0) (L.i32_type ctx.ir_context) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_cast_from_int32_to_uint32" f
    in

    let () = (* +(:bool): uint32 *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        (* unsigned, use zext *)
        L.build_zext_or_bitcast args.(0) (L.i32_type ctx.ir_context) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_cast_from_bool_to_uint32" f
    in
    ()
  in

  (* for int32 *)
  let () =
    let open Builtin_info in
    let init _ = L.const_int (L.i32_type ctx.ir_context) 0 in
    define_special_members int32_type_i init;

    let () = (* unary- (:int); int *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        L.build_neg args.(0) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_unary_pre_-_int" f
    in

    let () = (* +(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_+_int_int" f
    in
    let () = (* -(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_sub args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_-_int_int" f
    in
    let () = (* *(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_mul args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_*_int_int" f
    in
    let () = (* /(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_sdiv args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_/_int_int" f
    in
    let () = (* %(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_srem args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_%_int_int" f
    in
    let () = (* <(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Slt args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_<_int_int" f
    in
    let () = (* >(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Sgt args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_>_int_int" f
    in
    let () = (* |(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_or args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_|_int_int" f
    in
    let () = (* ^(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_xor args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_^_int_int" f
    in
    let () = (* &(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_and args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_&_int_int" f
    in
    let () = (* <=(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Sle args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_<=_int_int" f
    in
    let () = (* >=(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Sge args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_>=_int_int" f
    in
    let () = (* <<(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_shl args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_<<_int_int" f
    in
    let () = (* >>(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_ashr args.(0) args.(1) "" ctx.Ctx.ir_builder    (* sign ext(arithmetic) *)
      in
      register_builtin_func "__builtin_op_binary_>>_int_int" f
    in
    let () = (* >>>(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_lshr args.(0) args.(1) "" ctx.Ctx.ir_builder    (* zero ext(logical) *)
      in
      register_builtin_func "__builtin_op_binary_>>>_int_int" f
    in
    let () = (* ==(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Eq args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_==_int_int" f
    in
    let () = (* !=(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Ne args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_!=_int_int" f
    in
    ()
  in

  (* for uint32 *)
  let () =
    let open Builtin_info in
    let basename = "uint" in
    let init _ = L.const_int (L.i32_type ctx.ir_context) 0 in
    define_special_members uint32_type_i init;

    let () = (* +(:INT, :INT): INT *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_+_%s_%s" basename basename) f
    in
    let () = (* -(:INT, :INT): INT *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_sub args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_-_%s_%s" basename basename) f
    in
    let () = (* *(:INT, :INT): INT *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_mul args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_*_%s_%s" basename basename) f
    in
    let () = (* /(:INT, :INT): INT *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_udiv args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_/_%s_%s" basename basename) f
    in
    let () = (* %(:INT, :INT): INT *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_urem args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_%%_%s_%s" basename basename) f
    in
    let () = (* <(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Ult args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_<_%s_%s" basename basename) f
    in
    let () = (* >(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Ugt args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_>_%s_%s" basename basename) f
    in
    let () = (* |(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_or args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_|_%s_%s" basename basename) f
    in
    let () = (* ^(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_xor args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_^_%s_%s" basename basename) f
    in
    let () = (* &(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_and args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_&_%s_%s" basename basename) f
    in
    let () = (* <=(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Ule args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_<=_%s_%s" basename basename) f
    in
    let () = (* >=(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Uge args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_>=_%s_%s" basename basename) f
    in
    let () = (* <<(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_shl args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_<<_%s_%s" basename basename) f
    in
    let () = (* >>(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_lshr args.(0) args.(1) "" ctx.Ctx.ir_builder    (* zero ext(logical) *)
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_>>_%s_%s" basename basename) f
    in
    let () = (* >>>(:int, :int): int *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_lshr args.(0) args.(1) "" ctx.Ctx.ir_builder    (* zero ext(logical) *)
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_>>>_%s_%s" basename basename) f
    in
    let () = (* ==(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Eq args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_==_%s_%s" basename basename) f
    in
    let () = (* !=(:int, :int): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Ne args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func
        (Printf.sprintf "__builtin_op_binary_!=_%s_%s" basename basename) f
    in
    ()
  in

  (* for bool *)
  let () =
    let open Builtin_info in
    let init _ = L.const_int (L.i1_type ctx.ir_context) 0 in
    define_special_members bool_type_i init;
    let () = (* pre!(:bool): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 1);
        L.build_not args.(0) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_unary_pre_!_bool" f
    in
    let () = (* &&(:bool, :bool): bool *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_and args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_&&_bool_bool" f
    in
    ()
  in

  let () = (* *)
    let f _ _ args ctx =
      assert (Array.length args = 1);
      args.(0)
    in
    register_builtin_func "__builtin_make_ptr_from_ref" f
  in

  (* for ptr *)
  let () =
    let open Builtin_info in
    let init (ty, is_addr) =
      let llty = lltype_of_typeinfo ty ctx in
      L.const_pointer_null llty
    in
    define_special_members raw_ptr_type_i init;
    ()
  in

  (* for ptr *)
  let () =
    let open Builtin_info in
    let init (ty, is_addr) =
      let llty = lltype_of_typeinfo ty ctx in
      L.const_pointer_null llty
    in
    define_special_members untyped_raw_ptr_type_i init;

    let () = (* +(:raw_ptr!(T), :int): raw_ptr!(T) *)
      let f _ _ args ctx =
        assert (Array.length args = 2);
        L.build_in_bounds_gep args.(0) [|args.(1)|] "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_+_raw_ptr_int" f
    in

    let () = (* pre* (:raw_ptr!(T)): ref(T) *)
      let f template_args _ _ args ctx =
        assert (List.length template_args = 1);
        (*let ty_val = List.nth template_args 0 in
        let ty = match ty_val with
          | Ctfe_value.Type ty -> ty
          | _ -> failwith "[ICE]"
        in*)
        assert (Array.length args = 1);
        args.(0)
      in
      register_builtin_template_func "__builtin_op_unary_pre_*_raw_ptr" f
    in

    let () = (* ==(:raw_ptr!(T), :raw_ptr!(T)): bool *)
      let f _ _ _ args ctx =
        assert (Array.length args = 2);
        let res = L.build_ptrdiff args.(0) args.(1) "" ctx.Ctx.ir_builder in
        let zero = L.const_int (L.i64_type ctx.ir_context) 0 in
        L.build_icmp L.Icmp.Eq res zero "" ctx.Ctx.ir_builder
      in
      register_builtin_template_func "__builtin_op_binary_==_ptr_ptr" f
    in
    ()
  in
  ()

exception FailedToWriteBitcode
exception FailedToBuildBytecode

let create_object_from_ctx ctx options out_filepath =
  let open Ctx in

  let timer = Debug.Timer.create () in
  Debug.reportf "= GENERATE_OBJECT(%s)" out_filepath;

  let basic_name =
    try
      Filename.chop_extension out_filepath
    with
    | Invalid_argument _ -> out_filepath
  in
  let bitcode_name = basic_name ^ ".bc" in

  (* output LLVM bitcode to the file *)
  let bc_wrote = LBW.write_bitcode_file ctx.ir_module bitcode_name in
  if not bc_wrote then raise FailedToWriteBitcode;

  (* build bitcode and output object file *)
  let bin_name = basic_name ^ ".o" in
  let sc = Sys.command (Printf.sprintf "llc %s -filetype=obj -relocation-model=pic -o %s" (Filename.quote bitcode_name) (Filename.quote bin_name)) in
  if sc <> 0 then raise FailedToBuildBytecode;

  Debug.reportf "= GENERATE_OBJECT(%s) %s" out_filepath (Debug.Timer.string_of_elapsed timer);

  bin_name

let create_object node out_filepath ctx =
  inject_builtins ctx;

  let timer = Debug.Timer.create () in
  Debug.reportf "= GENERATE_CODE(%s)" out_filepath;
  let _ = generate_code node ctx in
  Debug.reportf "= GENERATE_CODE(%s) %s" out_filepath (Debug.Timer.string_of_elapsed timer);

  debug_dump_module ctx.Ctx.ir_module;
  create_object_from_ctx ctx [] out_filepath
