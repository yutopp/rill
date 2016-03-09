(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
module L = Llvm
module LBW = Llvm_bitwriter

type ('ty, 'ctx) record_t =
  | LLValue of L.llvalue
  | LLType of L.lltype
  | LLTypeGen of ('ty Ctfe_value.t list -> L.lltype)
  | BuiltinFunc of (L.llvalue array -> 'ctx -> L.llvalue)
  | TrivialAction

module CodeGeneratorType =
  struct
    type ir_context_t = L.llcontext
    type ir_builder_t = L.llbuilder
    type ir_module_t = L.llmodule
    type ir_intrinsics = Codegen_llvm_intrinsics.t

    type ('ty, 'ctx) value_record_t = ('ty, 'ctx) record_t
  end
module Ctx = Codegen_context.Make(CodeGeneratorType)
module TAst = Tagged_ast

type env_t = TAst.t Env.env_t
type type_info_t = env_t Type.info_t
type ctfe_val_t = type_info_t Ctfe_value.t
type ctx_t = (env_t, type_info_t, ctfe_val_t) Ctx.t
type type_gen_t = env_t Type.Generator.t

let agg_recever_name = "agg.reciever"


let make_default_context ?(opt_type_sets=None) ?(opt_uni_map=None) () =
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
    ~type_sets:opt_type_sets
    ~uni_map:opt_uni_map


let rec code_generate ~bb node ctx =
  let open Ctx in
  let open Codegen_llvm_intrinsics in
  match node with
  | TAst.Module (inner, pkg_names, mod_name, base_dir, _) ->
     begin
       code_generate ~bb:bb inner ctx
     end

  | TAst.StatementList (nodes) ->
     nodes |> List.iter @@ fun n -> code_generate ~bb:bb n ctx

  | TAst.ExprStmt e ->
     begin
       ignore @@ code_generate_as_value ~bb:bb e ctx
     end

  | TAst.ScopeStmt (stmts) ->
     begin
       code_generate ~bb:bb stmts ctx
     end

  | TAst.ReturnStmt (opt_e) ->
     begin
       match opt_e with
       | Some e ->
          let (llval, ty) = code_generate_as_value ~bb:bb e ctx in
          ignore @@ if is_address_representation ty then
                      L.build_ret_void ctx.ir_builder
                    else
                      L.build_ret llval ctx.ir_builder
       | None ->
          ignore @@ L.build_ret_void ctx.ir_builder
     end

  | TAst.GenericFuncDef (opt_body, Some env) ->
     if Ctx.is_env_defined ctx env then () else
     begin       (* *)
       let fenv_r = Env.FunctionOp.get_record env in
       match fenv_r.Env.fn_detail with
       | Env.FnRecordNormal (def, kind, fenv_d) ->
          begin
            let body = Option.get opt_body in
            (* if this class returns non primitive object, add a parameter to recieve the object *)
            let returns_heavy_obj =
              is_address_representation fenv_r.Env.fn_return_type
            in

            let llparam_tys =
              (if returns_heavy_obj then [fenv_r.Env.fn_return_type] else [])
              @ fenv_r.Env.fn_param_types
              |> List.map (fun t -> lltype_of_typeinfo_ac ~bb:bb t ctx)
              |> Array.of_list
            in
            let llret_ty =
              if returns_heavy_obj then
                L.void_type ctx.ir_context
              else
                lltype_of_typeinfo ~bb:bb fenv_r.Env.fn_return_type ctx
            in

            (* type of function *)
            let f_ty = L.function_type llret_ty llparam_tys in

            (* declare function *)
            let name = fenv_r.Env.fn_mangled |> Option.get in
            let f = L.declare_function name f_ty ctx.ir_module in
            Ctx.bind_val_to_env ctx (LLValue f) env;

            (* setup parameters *)
            let param_envs = fenv_d.Env.fn_n_param_envs |> List.enum in
            let ll_params = L.params f |> Array.enum in
            if returns_heavy_obj then begin
                                     let opt_agg = Enum.peek ll_params in
                                     assert (Option.is_some opt_agg);
                                     let agg = Option.get opt_agg in
                                     L.set_value_name agg_recever_name agg;
                                     Enum.drop 1 ll_params;
                                   end;
            let declare_param_var optenv llvar =
              match optenv with
              | Some env ->
                 begin
                   let venv = Env.VariableOp.get_record env in
                   let var_name = venv.Env.var_name in
                   L.set_value_name var_name llvar;
                   Ctx.bind_val_to_env ctx (LLValue llvar) env
                 end
              | None -> ()
            in
            Enum.iter2 declare_param_var param_envs ll_params;

            (* entry block *)
            let bb = L.append_block ctx.ir_context "entry" f in
            L.position_at_end bb ctx.ir_builder;

            (**)
            code_generate body ctx ~bb:(Some bb);

            L.dump_value f;
            flush_all ();
            Llvm_analysis.assert_valid_function f;

            Ctx.mark_env_as_defined ctx env
          end

       | Env.FnRecordTrivial (def, kind) ->
          begin
            let _ = match def with
              | Env.FnDefDefaulted -> ()   (* ACCEPT *)
              | Env.FnDefDeleted
              | Env.FnDefUserDefined -> failwith "[ICE]"
            in

            let r_value = match kind with
              | Env.FnKindDefaultConstructor
              | Env.FnKindCopyConstructor
              | Env.FnKindMoveConstructor
              | Env.FnKindConstructor -> TrivialAction
              | _ -> failwith "[ICE]"
            in
            Ctx.bind_val_to_env ctx r_value env;
            Ctx.mark_env_as_defined ctx env
          end

       | Env.FnRecordExternal (def, kind, extern_fname) ->
          begin
            let llparam_tys =
              fenv_r.Env.fn_param_types
              |> List.map (fun t -> lltype_of_typeinfo_ac ~bb:bb t ctx)
              |> Array.of_list
            in
            let llret_ty = lltype_of_typeinfo ~bb:bb fenv_r.Env.fn_return_type ctx in

            (* type of function *)
            let f_ty = L.function_type llret_ty llparam_tys in

            let llvm_func = L.declare_function extern_fname f_ty ctx.ir_module in

            Ctx.bind_val_to_env ctx (LLValue llvm_func) env;

            Ctx.mark_env_as_defined ctx env
          end

       | Env.FnRecordBuiltin (def, kind, name) ->
          begin
            (* DO NOTHING *)
          end

       | _ -> failwith "[ICE]"
     end

  | TAst.ClassDefStmt (
        name, body, opt_attr, Some env
      ) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       let cenv_r = Env.ClassOp.get_record env in

       (* define member variables *)
       let get_member_type env =
         let r = Env.VariableOp.get_record env in
         let llty = lltype_of_typeinfo ~bb:bb r.Env.var_type ctx in
         llty
       in
       let member_lltypes =
         List.map get_member_type cenv_r.Env.cls_member_vars
       in

       let struct_ty =
         L.named_struct_type ctx.Ctx.ir_context
                             (Option.get cenv_r.Env.cls_mangled)
       in
       L.struct_set_body struct_ty (Array.of_list member_lltypes)
                         false (* not packed *);

       Ctx.bind_val_to_env ctx (LLType struct_ty) env;

       Ctx.mark_env_as_defined ctx env
     end

  | TAst.ExternClassDefStmt (
        name, extern_cname, _, Some env
      ) ->
     begin
       if Ctx.is_env_defined ctx env then () else
       begin
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

         Ctx.mark_env_as_defined ctx env
       end
     end

  | TAst.VariableDefStmt (TAst.VarInit (var_init), Some env) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       let venv = Env.VariableOp.get_record env in
       let var_type = venv.Env.var_type in

       let (_, _, (_, opt_init_expr)) = var_init in
       let init_expr = Option.get opt_init_expr in

       Printf.printf "Define variable: %s\n" venv.Env.var_name;
       let (llval, expr_ty) = code_generate_as_value ~bb:bb init_expr ctx in
       Type.print var_type;
       flush_all ();

       L.set_value_name venv.Env.var_name llval;
       Ctx.bind_val_to_env ctx (LLValue llval) env;

       Ctx.mark_env_as_defined ctx env
     end

  | TAst.EmptyStmt -> ()

  | _ -> failwith "cannot generate : statement"

and code_generate_as_value ?(bb=None) node ctx : (L.llvalue * 'env Type.info_t)=
  let open Ctx in
  let open Codegen_llvm_intrinsics in
  match node with
  | TAst.GenericCallExpr (storage_ref, args, Some call_on_env, Some env) ->
     begin
       let f_er = Env.FunctionOp.get_record env in
       let {
         Env.fn_name = fn_name;
         Env.fn_detail = detail;
         Env.fn_param_types = param_tys;
         Env.fn_return_type = ret_ty;
       } = f_er in

       let returns_heavy_obj =
         is_address_representation ret_ty
       in

       let eval_args param_types =
         (* normal arguments *)
         let (llvals, tys) =
           args
           |> List.map (fun n -> code_generate_as_value ~bb:bb n ctx)
           |> List.split
         in
         (* special arguments *)
         let (llvals, tys, param_types) = match !storage_ref with
           | TAst.StoStack (ty) ->
              begin
                let llty = lltype_of_typeinfo ~bb:bb ty ctx in
                let v = L.build_alloca llty "" ctx.ir_builder in
                (v::llvals), (ty::tys), (ty::param_types)
              end

           | TAst.StoImm ->
              (llvals, tys, param_types)

           | _ -> (ignore llvals; ignore tys; failwith "")
         in
         let conv_funcs =
           List.map2 (fun t s -> adjust_llval_form ~bb:bb t s) param_types tys
         in
         (* TODO: conv *)
         let llargs = List.map2 (fun f v -> f v ctx) conv_funcs llvals
                      |> Array.of_list
         in
         llargs
       in

       let fn_s_name = Nodes.string_of_id_string fn_name in
       let llval = match detail with
         (* normal function *)
         | Env.FnRecordNormal (_, kind, _) ->
            begin
              Printf.printf "gen value: debug / function normal %s\n" fn_s_name;

              let r_value = force_target_generation ~bb:bb ctx env in
              match r_value with
              | LLValue f ->
                 begin
                   match kind with
                   | Env.FnKindFree ->
                      begin
                        match returns_heavy_obj with
                        | false ->
                           let llargs = eval_args param_tys in
                           L.build_call f llargs "" ctx.ir_builder
                        | true ->
                           let llargs = eval_args param_tys in
                           let _ = L.build_call f llargs "" ctx.ir_builder in
                           assert (Array.length llargs > 0);
                           llargs.(0)
                      end

                   | Env.FnKindConstructor ->
                      begin
                        failwith ""
                      end

                   | Env.FnKindMember ->
                      begin
                        failwith ""
                      end

                   | _ -> failwith "[ICE]"
                 end

              | _ -> failwith "[ICE]"
            end

         | Env.FnRecordTrivial (def, kind) ->
            begin
              Printf.printf "gen value: debug / function trivial %s\n" fn_s_name;

              let r_value = force_target_generation ~bb:bb ctx env in
              let _ = match r_value with
                | TrivialAction -> ()
                | _ -> failwith "[ICE]"
              in

              let new_obj = match !storage_ref with
                | TAst.StoStack (ty) ->
                   begin
                     let llty = lltype_of_typeinfo ~bb:bb ty ctx in
                     L.build_alloca llty "" ctx.ir_builder
                   end
                | TAst.StoAgg ->
                   begin
                     let ctx_env = Option.get call_on_env.Env.context_env in
                     Env.print ctx_env;
                     let ll_fval =
                       find_llval_by_env_with_force_generation ~bb:bb ctx ctx_env
                     in
                     let agg = L.param ll_fval 0 in
                     assert (L.value_name agg = agg_recever_name);
                     agg
                   end
                | _ -> failwith "[ICE]"
              in

              match kind with
              | Env.FnKindDefaultConstructor -> new_obj
              | Env.FnKindCopyConstructor
              | Env.FnKindMoveConstructor ->
                 begin
                   assert (List.length args = 1);
                   let rhs = List.hd args in
                   let (llrhs, _) = code_generate_as_value ~bb:bb rhs ctx in
                   let _ = ctx.intrinsics.memcpy_i32 new_obj llrhs
                                                     4 4 false ctx.ir_builder
                   in
                   new_obj
                 end
              | _ -> failwith "[ICE]"
            end

         (* external function *)
         | Env.FnRecordExternal (def, kind, extern_name) ->
            begin
              Printf.printf "gen value: debug / extern  %s = \"%s\"\n"
                            fn_s_name extern_name; flush_all ();
              let llargs = eval_args param_tys in
              let extern_f =
                find_llval_by_env_with_force_generation ~bb:bb ctx env
              in
              L.build_call extern_f llargs "" ctx.ir_builder
            end

         | Env.FnRecordBuiltin (def, kind, extern_name) ->
            begin
              Printf.printf "gen value: debug / builtin %s = \"%s\"\n"
                            fn_s_name extern_name;
              flush_all ();
              let llargs = eval_args param_tys in
              Array.iter L.dump_value llargs;

              let v_record = Ctx.find_val_by_name ctx extern_name in
              let builtin_gen_f = match v_record with
                | BuiltinFunc f -> f
                | _ -> failwith "[ICE]"
              in
              builtin_gen_f llargs ctx
            end

         | Env.FnUndef -> failwith "[ICE] codegen: undefined function record"
       in
       (llval, ret_ty)
     end

  | TAst.BoolLit (v, lit_ty) ->
     begin
       let llval = L.const_int (L.i1_type ctx.ir_context) (if v then 1 else 0) in
       (llval, lit_ty)
     end

  | TAst.Int32Lit (v, lit_ty) ->
     begin
       let llval = L.const_int (L.i32_type ctx.ir_context) v in
       (llval, lit_ty)
     end

  | TAst.ArrayLit (elems, lit_ty) ->
     begin
       (* XXX: adhoc implementation, fix it *)
       let (ll_elems, tys) =
         elems
         |> List.map (fun n -> code_generate_as_value ~bb:bb n ctx)
         |> List.split
       in
       let llty = lltype_of_typeinfo ~bb:bb lit_ty ctx in

       let llval = L.const_array llty (Array.of_list ll_elems) in
       (llval, lit_ty)
     end

  | TAst.GenericId (name, Some rel_env) ->
     begin
       let { Env.er = er; _ } = rel_env in
       match er with
       | Env.Function (_, r) ->
          begin
            (* *)
            Env.print rel_env;
            failwith @@ "[ICE] TAst.Id: function "
                        ^ (Nodes.string_of_id_string name)
                        ^ " // "
                        ^ (Nodes.string_of_id_string r.Env.fn_name)
          end

       | Env.Variable (r) ->
          begin
            Printf.printf "variable\n";
            let v_record = try Ctx.find_val_by_env ctx rel_env with
                           | Not_found -> failwith "[ICE] variable env is not found"
            in
            let llval = match v_record with
              | LLValue v -> v
              | _ -> failwith "[ICE]"
            in
            let ty = r.Env.var_type in
            (llval, ty)
          end

       | Env.Class (_, r) ->
          begin
            match ctx.type_sets with
            | Some tsets ->
               begin
                 let ty =
                   Type.Generator.generate_type tsets.Type_sets.ts_type_gen
                                                (Type_info.UniqueTy rel_env)
                                                r.Env.cls_template_vals
                                                Type_attr.undef
                 in
                 let itype_id = Option.get ty.Type_info.ti_id in
                 Printf.printf "##### type_id = %s\n" (Int64.to_string itype_id);

                 (* return the internal typeid as a type *)
                 let llval = L.const_of_int64 (L.i64_type ctx.ir_context)
                                              itype_id
                                              Type_info.is_type_id_signed
                 in
                 let ty = tsets.Type_sets.ts_type_type in
                 (llval, ty)
               end
            | None ->
               begin
                 Env.print rel_env;
                 failwith @@ "[ICE] TAst.Id: class in RUNTIME: "
                             ^ (Nodes.string_of_id_string name)
                             ^ " // "
                             ^ (Nodes.string_of_id_string r.Env.cls_name)
               end
          end

       (* regards all values are NOT references *)
       | Env.MetaVariable (uni_id) ->
          begin
            let fail_f () =
              failwith "[ICE] codegen_llvm : TAst.Id / meta variable in RUNTIME"
            in
            let uni_map = Option.default_delayed fail_f ctx.uni_map in
            let tsets = Option.default_delayed fail_f ctx.type_sets in

            Printf.printf "LLVM codegen Env.MetaVariable = %d\n" uni_id;
            let (_, c) = Unification.search_value_until_terminal uni_map uni_id in
            match c with
            | Unification.Val v ->
               begin
                 match v with
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
                              (llval, ty)
                           | None -> failwith "[ICE] codegen_llvm : has no type_id"
                         end
                      | _-> failwith "[ICE] codegen_llvm : type"
                    end

                 | Ctfe_value.Int32 i32 ->
                    let llval =
                      L.const_of_int64 (L.i32_type ctx.ir_context)
                                       (Int32.to_int64 i32)
                                       false (* not signed *)
                    in
                    let ty = !(tsets.Type_sets.ts_int32_type_holder) in
                    (llval, ty)

                 | Ctfe_value.Undef ud_uni_id ->
                    raise @@ Ctfe_exn.Meta_var_un_evaluatable ud_uni_id

               end
            | _ -> raise @@ Ctfe_exn.Meta_var_un_evaluatable uni_id
          end

       | _ ->
          begin
            Env.print rel_env;
            failwith @@ "codegen; id " ^ (Nodes.string_of_id_string name)
          end
     end

  | _ -> failwith "cannot generate : as value"


and code_generate_by_interrupt ~bb:bb node ctx =
  code_generate ~bb:bb node ctx;
  (* Restore position *)
  bb |> Option.may (fun bb -> L.position_at_end bb ctx.Ctx.ir_builder)


and force_target_generation ~bb ctx env =
  try Ctx.find_val_by_env ctx env with
  | Not_found ->
     begin
       let { Env.rel_node = rel_node; _ } = env in
       let node = match rel_node with
         | Some v -> v
         | None ->
            begin
              Env.print env;
              failwith "[ICE] force_target_generation: there is no rel node"
            end
       in
       code_generate_by_interrupt ~bb:bb node ctx;

       (* retry *)
       try Ctx.find_val_by_env ctx env with
       | Not_found ->
          begin
            Env.print env;
            failwith "[ICE] force_target_generation: couldn't find target"
          end
     end

and find_llval_by_env_with_force_generation ~bb ctx env =
  let v_record = force_target_generation ~bb:bb ctx env in
  match v_record with
  | LLValue v -> v
  | _ -> failwith "[ICE] / find_llval_by_env_with_force_generation"

and find_lltype_by_env_with_force_generation ~bb ctx env =
  let v_record = force_target_generation ~bb:bb ctx env in
  match v_record with
  | LLType t -> t
  | _ -> failwith "[ICE] / find_lltype_by_env_with_force_generation"


and lltype_of_typeinfo ~bb ty ctx =
  let open Ctx in
  let cenv = Type.as_unique ty in
  let ll_ty = find_lltype_by_env_with_force_generation ~bb:bb ctx cenv in
  ll_ty

and lltype_of_typeinfo_ac ~bb ty ctx =
  let ll_ty = lltype_of_typeinfo ~bb:bb ty ctx in
  if is_address_representation ty then
    L.pointer_type ll_ty
  else
    ll_ty


and is_heavy_class ty =
  match Type.type_sort ty with
  | Type_info.UniqueTy cenv ->
     begin
     end
  | _ -> failwith "[ICE] / is_heavy_class"

(**)
and is_address_representation ty =
  let {
    Type_attr.ta_ref_val = rv;
    Type_attr.ta_mut = mut;
  } = ty.Type_info.ti_attr in
  match rv with
  | Type_attr.Ref
  | Type_attr.Val ->
     begin
       match mut with
       | Type_attr.Mutable -> true
       | Type_attr.Const
       | Type_attr.Immutable ->
          begin
            let cenv = Type.as_unique ty in
            let cr = Env.ClassOp.get_record cenv in
            let traits = Option.get cr.Env.cls_traits in
            if traits.Env.cls_traits_is_primitive then
              false
            else
              true
          end
       | _ -> failwith "[ICE] Unexpected : mut"
     end
  | _ -> failwith "[ICE] Unexpected : rv"

and adjust_llval_form ~bb trg_ty src_ty llval ctx =
  let open Ctx in
  Printf.printf "is_pointer rep?  trg: %b, src: %b\n"
                (is_address_representation trg_ty)
                (is_address_representation src_ty);
  L.dump_value llval;
  flush_all ();
  match (is_address_representation trg_ty, is_address_representation src_ty) with
  | (true, true)
  | (false, false) -> llval
  | (true, false) ->
     let llty = lltype_of_typeinfo ~bb:bb trg_ty ctx in
     let v = L.build_alloca llty "" ctx.ir_builder in
     let _ = L.build_store llval v ctx.ir_builder in
     v
  | (false, true) ->
     L.build_load llval "" ctx.ir_builder


let regenerate_module ctx =
  let ir_module = L.create_module ctx.Ctx.ir_context "Rill" in
  ctx.Ctx.ir_module <- ir_module


let inject_builtins ctx =
  let open Ctx in
  let register_builtin_type name record =
    Ctx.bind_val_to_name ctx record name;
    Printf.printf "debug / registerd builtin type = \"%s\"\n" name
  in
  let register_builtin_func name f =
    Ctx.bind_val_to_name ctx (BuiltinFunc f) name;
    Printf.printf "debug / registerd builtin func = \"%s\"\n" name
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
    register_builtin_type int32_type_i.internal_name
                          (LLType (L.i32_type ctx.ir_context));

    register_builtin_type array_type_i.internal_name
                          (LLTypeGen (
                               fun args ->
                               begin
                                 assert (List.length args = 2);
                                 let ty_ct_val = List.nth args 0 in
                                 let len_ct_val = List.nth args 1 in
                                 let ty_val = match ty_ct_val with
                                   | Ctfe_value.Type ty -> ty
                                   | _ -> failwith "[ICE]"
                                 in
                                 let len_val = match len_ct_val with
                                   | Ctfe_value.Int32 i32 -> i32
                                   | _ -> failwith "[ICE]"
                                 in
                                 (* TODO: fix *)
                                 let llty = lltype_of_typeinfo ~bb:None ty_val ctx in
                                 let len = Int32.to_int len_val in
                                 L.array_type llty len
                               end
                             ));
  end;

  (*
   * Builtin functions
   *)
  let () =
    let add_int_int args ctx =
      assert (Array.length args = 2);
      L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
    in
    register_builtin_func "__builtin_op_binary_+_int_int" add_int_int;
  in

  (* for int32 *)
  let () =
    let open Builtin_info in
    let () = (* default constructor *)
      let f args ctx =
        let v = L.const_int (L.i32_type ctx.ir_context) 0 in
        match Array.length args with
        | 1 -> let _ = L.build_store v args.(0) ctx.ir_builder in args.(0)
        | 0 -> v
        | _ -> failwith ""
      in
      register_builtin_func
        (make_builtin_default_ctor_name int32_type_i.internal_name) f
    in
    let () = (* copy constructor *)
      let f args ctx =
        match Array.length args with
        | 2 -> let _ = L.build_store args.(1) args.(0) ctx.ir_builder in args.(0)
        | 1 -> args.(0)
        | _ -> failwith ""
      in
      register_builtin_func
        (make_builtin_copy_ctor_name int32_type_i.internal_name) f
    in
    let () =
      let f args ctx =
        assert (Array.length args = 2);
        L.build_store args.(1) args.(0) ctx.Ctx.ir_builder
      in
      register_builtin_func
        (make_builtin_copy_assign_name int32_type_i.internal_name) f
    in
    ()
  in
  ()


let generate node ctx =
  inject_builtins ctx;

  code_generate ~bb:None node ctx;
  L.dump_module ctx.Ctx.ir_module


exception FailedToWriteBitcode
exception FailedToBuildBitcode
exception FailedToBuildExecutable

let create_executable ctx lib_name out_name =
  let open Ctx in

  let basic_name = try Filename.chop_extension out_name with Invalid_argument _ -> out_name in
  let bitcode_name = basic_name ^ ".bc" in

  (* output LLVM bitcode to the file *)
  let bc_wrote = LBW.write_bitcode_file ctx.ir_module bitcode_name in
  if not bc_wrote then raise FailedToWriteBitcode;

  (* build bitcode and output object file *)
  let bin_name = basic_name ^ ".o" in
  let sc = Sys.command (Printf.sprintf "llc %s -filetype=obj -o %s " (Filename.quote bitcode_name) (Filename.quote bin_name)) in
  if sc <> 0 then raise FailedToBuildBitcode;

  (* output executable *)
  let sc = Sys.command (Printf.sprintf "g++ %s %s -o %s" (Filename.quote bin_name) (Filename.quote lib_name) (Filename.quote out_name)) in
  (*let sc = Sys.command (Printf.sprintf "gcc %s -o %s" (Filename.quote bin_name) (Filename.quote out_name)) in*)
  if sc <> 0 then raise FailedToBuildExecutable;
  ()
