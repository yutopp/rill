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
  | ElemIndex of int
  | BuiltinFunc of (L.llvalue array -> 'ctx -> L.llvalue)
  | BuiltinFuncGen of ('ty Ctfe_value.t list -> L.llvalue array -> 'ctx -> L.llvalue)
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
type type_gen_t = env_t Type.Generator.t
type ctx_t = (env_t, type_info_t, ctfe_val_t) Ctx.t

let agg_recever_name = "agg.receiver"


let make_default_context ~type_sets ~uni_map =
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


let rec generate_code node ctx : (L.llvalue * 'env Type.info_t) =
  let open Ctx in
  let void_t = L.void_type ctx.ir_context in
  let void_v = L.undef void_t in
  let void_val = (void_v, ctx.type_sets.Type_sets.ts_void_type) in

  match node with
  | TAst.Module (inner, pkg_names, mod_name, base_dir, _) ->
     begin
       generate_code inner ctx
     end

  | TAst.StatementList (nodes) ->
     let rec gen nx (v, t) = match nx with
       | [] -> (v, t)
       | x :: xs ->
          let l = generate_code x ctx in
          gen xs l
     in
     gen nodes void_val

  | TAst.ExprStmt e ->
     begin
       generate_code e ctx
     end

(*  | TAst.ScopeStmt (stmts) ->
     begin
       generate_code ~bb:bb stmts ctx
     end
 *)

  | TAst.ReturnStmt (opt_e) ->
     begin
       Printf.printf "ReturnStmt!!!!!!!!\n";
       flush_all ();
       let _ = match opt_e with
       | Some e ->
          let (llval, ty) = generate_code e ctx in
          ignore @@ if is_heavy_object ty then
                      L.build_ret_void ctx.ir_builder
                    else
                      L.build_ret llval ctx.ir_builder
       | None ->
          ignore @@ L.build_ret_void ctx.ir_builder
       in
       void_val
     end

  | TAst.GenericFuncDef (opt_body, Some env) ->
     if Ctx.is_env_defined ctx env then void_val else
     begin
       Ctx.mark_env_as_defined ctx env;

       let fenv_r = Env.FunctionOp.get_record env in
       match fenv_r.Env.fn_detail with
       | Env.FnRecordNormal (def, kind, fenv_d) ->
          begin
            let body = Option.get opt_body in

            (* if this class returns non primitive object, add a parameter to receive the object *)
            let returns_heavy_obj = is_heavy_object fenv_r.Env.fn_return_type in

            let func_ret_ty =
              if returns_heavy_obj then
                ctx.type_sets.Type_sets.ts_void_type
              else
                fenv_r.Env.fn_return_type
            in
            let llret_ty = lltype_of_typeinfo_ret func_ret_ty ctx in

            let llparam_tys =
              (if returns_heavy_obj then [fenv_r.Env.fn_return_type] else [])
              @ fenv_r.Env.fn_param_types
              |> List.map (fun t -> lltype_of_typeinfo_param t ctx)
              |> Array.of_list
            in

            (* type of function *)
            let f_ty = L.function_type llret_ty llparam_tys in

            (* declare function *)
            let name = fenv_r.Env.fn_mangled |> Option.get in
            let f = L.declare_function name f_ty ctx.ir_module in
            Ctx.bind_val_to_env ctx (LLValue f) env;

            (* entry block *)
            let eib = L.append_block ctx.ir_context "entry" f in
            L.position_at_end eib ctx.ir_builder;

            (* setup parameters *)
            let param_envs = fenv_d.Env.fn_n_param_envs |> List.enum in
            let raw_ll_params = L.params f |> Array.enum in
            if returns_heavy_obj then begin
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
                                          begin
                                            let venv_r = Env.VariableOp.get_record venv in
                                            let var_name = venv_r.Env.var_name in
                                            L.set_value_name var_name agg;
                                            Ctx.bind_val_to_env ctx (LLValue agg) venv
                                          end
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
                 v
              | (true, true)
              | (false, false) -> llval
              | _ -> failwith "[ICE]"
            in
            let ll_params =
              Enum.combine ((List.enum fenv_r.Env.fn_param_types), raw_ll_params)
              |> Enum.map adjust_param_type
            in
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
            L.position_at_end eib ctx.ir_builder;

            (**)
            let _ = generate_code body ctx in

            (**)
            if not env.Env.closed && Type.has_same_class func_ret_ty (ctx.type_sets.Type_sets.ts_void_type) then
              ignore @@ L.build_ret_void ctx.ir_builder;

            L.dump_value f;
            Printf.printf "generated genric function(%b): %s\n" env.Env.closed name;
            flush_all ();

            Llvm_analysis.assert_valid_function f;
            void_val
          end

       | Env.FnRecordImplicit (def, kind) ->
          begin
            let _ = match def with
              | Env.FnDefDefaulted true -> ()   (* ACCEPT *)
              | _ -> failwith "[ICE]"
            in

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

       | Env.FnRecordExternal (def, kind, extern_fname) ->
          begin
            let llparam_tys =
              fenv_r.Env.fn_param_types
              |> List.map (fun t -> lltype_of_typeinfo_param t ctx)
              |> Array.of_list
            in
            let llret_ty = lltype_of_typeinfo fenv_r.Env.fn_return_type ctx in

            (* type of function *)
            let f_ty = L.function_type llret_ty llparam_tys in

            let llvm_func = L.declare_function extern_fname f_ty ctx.ir_module in

            L.dump_value llvm_func;
            flush_all ();

            Ctx.bind_val_to_env ctx (LLValue llvm_func) env;
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
        name, body, opt_attr, Some env
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

       (* body *)
       let _ = generate_code body ctx in
       void_val
     end

  | TAst.ExternClassDefStmt (
        name, extern_cname, _, Some env
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

       Printf.printf "Define variable: %s\n" venv.Env.var_name;
       let (llval, expr_ty) = generate_code init_expr ctx in
       Type.print var_type;
       flush_all ();

       L.set_value_name venv.Env.var_name llval;
       Ctx.bind_val_to_env ctx (LLValue llval) env;

       Ctx.mark_env_as_defined ctx env;
       void_val
     end

  | TAst.MemberVariableDefStmt _ -> void_val
  | TAst.EmptyStmt -> void_val

  | TAst.GenericCallExpr (storage_ref, args, Some call_on_env, Some env) ->
     begin
       let f_er = Env.FunctionOp.get_record env in
       let {
         Env.fn_name = fn_name;
         Env.fn_detail = detail;
         Env.fn_param_types = param_tys;
         Env.fn_return_type = ret_ty;
       } = f_er in

       let returns_heavy_obj = is_heavy_object ret_ty in

       let eval_args param_types =
         (* normal arguments *)
         let (llvals, tys) =
           args
           |> List.map (fun n -> generate_code n ctx)
           |> List.split
         in
         (* special arguments *)
         let (llvals, tys, param_types) = match !storage_ref with
           | TAst.StoStack (ty) ->
              begin
                let llty = lltype_of_typeinfo ty ctx in
                let v = L.build_alloca llty "" ctx.ir_builder in
                (v::llvals), (ty::tys), (ty::param_types)
              end

           | TAst.StoImm ->
              (llvals, tys, param_types)

           | _ -> (ignore llvals; ignore tys; failwith "")
         in
         let conv_funcs =
           let f t s = adjust_arg_llval_form t s in
           List.map2 f param_types tys
         in
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

              let r_value = force_target_generation ctx env in
              match r_value with
              | LLValue f ->
                 begin
                   match kind with
                   | Env.FnKindFree
                   | Env.FnKindMember
                   | Env.FnKindConstructor _ ->
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

                   | _ -> failwith "[ICE]"
                 end

              | _ -> failwith "[ICE]"
            end

         | Env.FnRecordImplicit (def, kind) ->
            begin
              let open Codegen_llvm_intrinsics in
              Printf.printf "gen value: debug / function implicit %s\n" fn_s_name;

              let r_value = force_target_generation ctx env in
              let _ = match r_value with
                | TrivialAction -> ()
                | _ -> failwith "[ICE]"
              in

              let new_obj = match !storage_ref with
                | TAst.StoStack (ty) ->
                   begin
                     let llty = lltype_of_typeinfo ty ctx in
                     L.build_alloca llty "" ctx.ir_builder
                   end
                | TAst.StoAgg ->
                   begin
                     let ctx_env = Option.get call_on_env.Env.context_env in
                     Env.print ctx_env;
                     let ll_fval =
                       find_llval_by_env_with_force_generation ctx ctx_env
                     in
                     let agg = L.param ll_fval 0 in
                     assert (L.value_name agg = agg_recever_name);
                     agg
                   end
                | _ -> failwith "[ICE]"
              in

              match kind with
              | Env.FnKindDefaultConstructor None -> new_obj
              | Env.FnKindCopyConstructor None
              | Env.FnKindMoveConstructor None ->
                 begin
                   assert (List.length args = 1);
                   let rhs = List.hd args in
                   let (llrhs, _) = generate_code rhs ctx in
                   let sl = Int64.of_int 8 in
                   (*let al = L.int64_of_const (L.align_of (L.type_of llrhs)) |> Option.get in*)
                   let al = Int64.of_int 0 in
                   let _ = ctx.intrinsics.memcpy_i32 new_obj llrhs
                                                     sl al false ctx.ir_builder (* TODO: fix *)
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
                find_llval_by_env_with_force_generation ctx env
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
                | BuiltinFuncGen f ->
                   let fenv_r = Env.FunctionOp.get_record env in
                   f fenv_r.Env.fn_template_vals
                | _ -> failwith "[ICE]"
              in
              builtin_gen_f llargs ctx
            end

         | Env.FnUndef -> failwith "[ICE] codegen: undefined function record"
       in
       (llval, ret_ty)
     end

  | TAst.NestedExpr (lhs_node, _, rhs_ty, Some rhs_env) ->
     begin
       let (ll_lhs, _) = generate_code lhs_node ctx in

       let v_record = try Ctx.find_val_by_env ctx rhs_env with
                      | Not_found -> failwith "[ICE] member env is not found"
       in
       let index = match v_record with
         | ElemIndex i -> i
         | _ -> failwith "[ICE]"
       in
       let llelem = L.build_struct_gep ll_lhs index "" ctx.ir_builder in
       (llelem, rhs_ty)
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

  | TAst.StringLit (str, lit_ty) ->
     begin
       let llval = L.build_global_stringptr str "" ctx.ir_builder in
       (llval, lit_ty)
     end

  | TAst.ArrayLit (elems, arr_ty) ->
     begin
       (* TODO: support static storages *)
       let arr_llty = lltype_of_typeinfo arr_ty ctx in
       let arr = L.build_alloca arr_llty "" ctx.ir_builder in
(*
       (* XXX: adhoc implementation, fix it *)
       let (ll_elems, tys) =
         elems
         |> List.map (fun n -> generate_code n ctx)
         |> List.split
       in
       let llty = lltype_of_typeinfo lit_ty ctx in

       let llval = L.const_array llty (Array.of_list ll_elems) in*)
       (arr, arr_ty)
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
                           | Not_found ->
                              begin
                                try Ctx.find_metaval_by_env ctx rel_env with
                                | Not_found -> failwith "[ICE] variable env is not found"
                              end
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
            let ty_attr_val_default = {
              Type_attr.ta_ref_val = Type_attr.Val;
              Type_attr.ta_mut = Type_attr.Const;
            } in
            let ty =
              Type.Generator.generate_type ctx.type_sets.Type_sets.ts_type_gen
                                           (Type_info.UniqueTy rel_env)
                                           r.Env.cls_template_vals
                                           ty_attr_val_default
            in
            let itype_id = Option.get ty.Type_info.ti_id in
            Printf.printf "##### type_id = %s\n" (Int64.to_string itype_id);

            (* return the internal typeid as a type *)
            let llval = L.const_of_int64 (L.i64_type ctx.ir_context)
                                         itype_id
                                         Type_info.is_type_id_signed
            in
            let ty = ctx.type_sets.Type_sets.ts_type_type in
            (llval, ty)
          end

       (* regards all values are NOT references *)
       | Env.MetaVariable (uni_id) ->
          begin
            let uni_map = ctx.uni_map in

            Printf.printf "LLVM codegen Env.MetaVariable = %d\n" uni_id;
            let (_, c) = Unification.search_value_until_terminal uni_map uni_id in
            match c with
            | Unification.Val v ->  ctfe_val_to_llval v ctx
            | _ -> raise @@ Ctfe_exn.Meta_var_un_evaluatable uni_id
          end

       | _ ->
          begin
            Env.print rel_env;
            failwith @@ "codegen; id " ^ (Nodes.string_of_id_string name)
          end
     end

  | TAst.ScopeExpr (block) ->
     begin
       generate_code block ctx
     end

  | TAst.IfExpr (cond_expr, then_expr, opt_else_expr) ->
     begin
       let (llcond, _) = generate_code cond_expr ctx in

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
       let (then_llval, then_ty) = generate_code then_expr ctx in
       let then_branch = if not (L.is_terminator then_llval) then
                           ignore @@ L.build_br fip ctx.ir_builder;
                         let cip = L.insertion_block ctx.ir_builder in
                         (then_llval, cip)
       in

       let brs = match opt_else_expr with
         | Some else_expr ->
            begin
              L.position_at_end eip ctx.Ctx.ir_builder;
              let (else_llval, _) = generate_code else_expr ctx in
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
       let llret = L.build_phi brs "" ctx.ir_builder in
       (llret, then_ty)
     end

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
              let (llcond, _) = generate_code cond ctx in
              ignore @@ L.build_cond_br llcond sip eip ctx.ir_builder;
              L.position_at_end sip ctx.Ctx.ir_builder;
            end
         | None -> L.remove_block sip;
       in

       ignore @@ generate_code body ctx;

       let _ = match opt_step with
         | Some step ->
            begin
              ignore @@  generate_code step ctx
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
       Printf.printf "##### type_id = %s\n" (Int64.to_string itype_id);

       (* return the internal typeid as a type *)
       let llval = L.const_of_int64 (L.i64_type ctx.ir_context)
                                    itype_id
                                    Type_info.is_type_id_signed
       in
       let ty = ctx.type_sets.Type_sets.ts_type_type in
       (llval, ty)
     end

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
               (llval, ty)
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
     (llval, ty)

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
              Env.print env;
              failwith "[ICE] force_target_generation: there is no rel node"
            end
       in
       generate_code_by_interrupt node ctx;

       (* retry *)
       try Ctx.find_val_by_env ctx env with
       | Not_found ->
          begin
            Env.print env;
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


and register_metaval value env ctx =
  let (llval, _) = ctfe_val_to_llval value ctx in
  let cv = LLValue llval in
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


and is_heavy_object ty =
  let {
    Type_attr.ta_ref_val = rv;
    Type_attr.ta_mut = mut;
  } = ty.Type_info.ti_attr in
  match rv with
  | Type_attr.Ref -> false
  | Type_attr.Val -> is_address_representation ty
  | _ -> failwith "[ICE] Unexpected : rv"

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
            (* if type is primitive, it will not be represented by address *)
            not (is_primitive ty)
          end
       | _ -> failwith "[ICE] Unexpected : mut"
     end
  | _ -> failwith "[ICE] Unexpected : rv"

and is_address_representation_param ty =
  let {
    Type_attr.ta_ref_val = rv;
  } = ty.Type_info.ti_attr in
  match rv with
  | Type_attr.Val when is_primitive ty -> false
  | _ -> is_address_representation ty

and adjust_llval_form' trg_ty trg_chkf src_ty src_chkf llval ctx =
  let open Ctx in
  match (trg_chkf trg_ty, src_chkf src_ty) with
  | (true, true)
  | (false, false) -> llval
  | (true, false) ->
     let llty = lltype_of_typeinfo trg_ty ctx in
     let v = L.build_alloca llty "" ctx.ir_builder in
     let _ = L.build_store llval v ctx.ir_builder in
     v
  | (false, true) ->
     L.build_load llval "" ctx.ir_builder

and adjust_llval_form trg_ty src_ty llval ctx =
  Type.print trg_ty;
  Type.print src_ty;
  Printf.printf "is_pointer rep?  trg: %b, src: %b\n src = "
                (is_address_representation trg_ty)
                (is_address_representation src_ty);
  flush_all ();
  L.dump_value llval;
  flush_all ();

  let trg_check_f = is_address_representation in
  let src_check_f = is_address_representation in
  adjust_llval_form' trg_ty trg_check_f src_ty src_check_f llval ctx

and adjust_arg_llval_form trg_ty src_ty llval ctx =
  if is_primitive trg_ty then
    begin
      Type.print trg_ty;
      Type.print src_ty;
      Printf.printf "is_pointer_arg rep?  trg: %b, src: %b\n src = "
                    (is_address_representation_param trg_ty)
                    (is_address_representation src_ty);
      flush_all ();
      L.dump_value llval;
      flush_all ();

      let trg_check_f = is_address_representation_param in
      let src_check_f = is_address_representation in
      adjust_llval_form' trg_ty trg_check_f src_ty src_check_f llval ctx
    end
  else
    adjust_llval_form trg_ty src_ty llval ctx


let regenerate_module ctx =
  let ir_module = L.create_module ctx.Ctx.ir_context "Rill" in
  ctx.Ctx.ir_module <- ir_module


let inject_builtins ctx =
  let open Ctx in
  let register_builtin_type name record =
    Ctx.bind_val_to_name ctx record name;
    (*Printf.printf "debug / registerd builtin type = \"%s\"\n" name*)
  in
  let register_builtin_func name f =
    Ctx.bind_val_to_name ctx (BuiltinFunc f) name;
    (*Printf.printf "debug / registerd builtin func = \"%s\"\n" name*)
  in
  let register_builtin_template_func name f =
    Ctx.bind_val_to_name ctx (BuiltinFuncGen f) name;
    (*Printf.printf "debug / registerd builtin func = \"%s\"\n" name*)
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
                                   | Ctfe_value.Int32 i32 -> i32
                                   | _ -> failwith "[ICE]"
                                 in
                                 (* TODO: fix *)
                                 let llty = lltype_of_typeinfo ty_val ctx in
                                 let len = Int32.to_int len_val in
                                 L.array_type llty len
                               end
                             ));
  end;

  (*
   * Builtin functions
   *)
  let () =
    let f template_args args ctx =
      assert (List.length template_args = 1);
      let ty_val = List.nth template_args 0 in
      let ty = match ty_val with
        | Ctfe_value.Type ty -> ty
        | _ -> failwith "[ICE]"
      in
      let env = Type.as_unique ty in
      let cenv_r = Env.ClassOp.get_record env in
      L.const_int (L.i32_type ctx.ir_context) cenv_r.Env.cls_size
    in
    register_builtin_template_func "__builtin_sizeof" f
  in

  let () =
    let f template_args args ctx =
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
    let f template_args args ctx =
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
    let f template_args args ctx =
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
    let f args ctx =
      let open Codegen_llvm_intrinsics in
      assert (Array.length args = 2);

      let to_obj = args.(0) in
      let from_obj = args.(1) in
      let size_of = Int64.of_int 8 in   (* TODO: fix fix fix !!!!! *)
      let align_of = Int64.of_int 0 in
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

  let define_special_members builtin_info init =
    let open Builtin_info in
    let () = (* default constructor *)
      let f args ctx =
        let v = init in
        match Array.length args with
        | 1 -> let _ = L.build_store v args.(0) ctx.ir_builder in args.(0)
        | 0 -> v
        | _ -> failwith ""
      in
      register_builtin_func
        (make_builtin_default_ctor_name builtin_info.internal_name) f
    in
    let () = (* copy constructor *)
      let f args ctx =
        match Array.length args with
        | 2 -> let _ = L.build_store args.(1) args.(0) ctx.ir_builder in args.(0)
        | 1 -> args.(0)
        | _ -> failwith ""
      in
      register_builtin_func
        (make_builtin_copy_ctor_name builtin_info.internal_name) f
    in
    let () = (* copy assign *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_store args.(1) args.(0) ctx.Ctx.ir_builder
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

  (* for int8 *)
  let () =
    let open Builtin_info in
    let init = L.const_int (L.i8_type ctx.ir_context) 0 in
    define_special_members uint8_type_i init;
    ()
  in

  (* for int32 *)
  let () =
    let open Builtin_info in
    let init = L.const_int (L.i32_type ctx.ir_context) 0 in
    define_special_members int32_type_i init;

    let () = (* +(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_+_int_int" f
    in
    let () = (* -(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_sub args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_-_int_int" f
    in
    let () = (* *(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_mul args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_*_int_int" f
    in
    let () = (* /(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_sdiv args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_/_int_int" f
    in
    let () = (* %(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_srem args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_%_int_int" f
    in
    let () = (* <(:int, :int): bool *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Slt args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_<_int_int" f
    in
    let () = (* >(:int, :int): bool *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Sgt args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_>_int_int" f
    in
    let () = (* |(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_or args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_|_int_int" f
    in
    let () = (* ^(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_xor args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_^_int_int" f
    in
    let () = (* &(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_and args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_&_int_int" f
    in
    let () = (* <=(:int, :int): bool *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Sge args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_<=_int_int" f
    in
    let () = (* >=(:int, :int): bool *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Sge args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_>=_int_int" f
    in
    let () = (* <<(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_shl args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_<<_int_int" f
    in
    let () = (* >>(:int, :int): int *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_lshr args.(0) args.(1) "" ctx.Ctx.ir_builder    (* zero ext(logical) *)
      in
      register_builtin_func "__builtin_op_binary_>>_int_int" f
    in
    let () = (* ==(:int, :int): bool *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Eq args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_==_int_int" f
    in
    let () = (* !=(:int, :int): bool *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_icmp L.Icmp.Ne args.(0) args.(1) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_!=_int_int" f
    in
    ()
  in

  (* for bool *)
  let () =
    let open Builtin_info in
    let init = L.const_int (L.i1_type ctx.ir_context) 0 in
    define_special_members bool_type_i init;
    let () = (* pre!(:bool): bool *)
      let f args ctx =
        assert (Array.length args = 1);
        L.build_not args.(0) "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_unary_pre_!_bool" f
    in
    ()
  in

  (* for ptr *)
  let () =
    let open Builtin_info in
    let init = L.const_int (L.i1_type ctx.ir_context) 0 in
    define_special_members raw_ptr_type_i init;
    ()
  in

  (* for ptr *)
  let () =
    let open Builtin_info in
    let init = L.const_int (L.i1_type ctx.ir_context) 0 in
    define_special_members untyped_raw_ptr_type_i init;

    let () = (* +(:raw_ptr!(T), :int): raw_ptr!(T) *)
      let f args ctx =
        assert (Array.length args = 2);
        L.build_in_bounds_gep args.(0) [|args.(1)|] "" ctx.Ctx.ir_builder
      in
      register_builtin_func "__builtin_op_binary_+_raw_ptr_int" f
    in

    let () = (* pre* (:raw_ptr!(T)): ref(T) *)
      let f template_args args ctx =
        assert (List.length template_args = 1);
        let ty_val = List.nth template_args 0 in
        let ty = match ty_val with
          | Ctfe_value.Type ty -> ty
          | _ -> failwith "[ICE]"
        in
        assert (Array.length args = 1);
        if is_address_representation ty then
          args.(0)
        else
          L.build_load args.(0) "" ctx.ir_builder
      in
      register_builtin_template_func "__builtin_op_unary_pre_*_raw_ptr" f
    in
    ()
  in

  ()


let generate node ctx =
  inject_builtins ctx;

  let _ = generate_code node ctx in
  L.dump_module ctx.Ctx.ir_module


exception FailedToWriteBitcode
exception FailedToBuildBitcode
exception FailedToBuildExecutable

let create_executable ctx link_options out_name =
  let open Ctx in

  let basic_name = try Filename.chop_extension out_name with Invalid_argument _ -> out_name in
  let bitcode_name = basic_name ^ ".bc" in

  (* output LLVM bitcode to the file *)
  let bc_wrote = LBW.write_bitcode_file ctx.ir_module bitcode_name in
  if not bc_wrote then raise FailedToWriteBitcode;

  (* build bitcode and output object file *)
  let bin_name = basic_name ^ ".o" in
  let sc = Sys.command (Printf.sprintf "llc %s -filetype=obj -o %s " (Filename.quote bitcode_name) (Filename.quote bin_name)) in
  flush_all ();
  if sc <> 0 then raise FailedToBuildBitcode;

  (* output executable *)
  let cmd = Printf.sprintf "g++ %s %s -o %s" (Filename.quote bin_name) link_options (Filename.quote out_name) in
  Printf.printf "cmd = %s\n" cmd; flush_all ();
  let sc = Sys.command cmd in
  flush_all ();
  (*let sc = Sys.command (Printf.sprintf "gcc %s -o %s" (Filename.quote bin_name) (Filename.quote out_name)) in*)
  if sc <> 0 then raise FailedToBuildExecutable;
  ()
