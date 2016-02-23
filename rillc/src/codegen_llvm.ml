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

module CodeGeneratorType =
  struct
    type ir_context_t = L.llcontext
    type ir_builder_t = L.llbuilder
    type ir_module_t = L.llmodule

    type ir_value_t = L.llvalue
    type ir_type_t = L.lltype

    type 'ctx builtin_f_t = (ir_value_t array -> 'ctx -> ir_value_t)
  end
module Ctx = Codegen_context.Make(CodeGeneratorType)
module TAst = Tagged_ast

type env_t = TAst.t Env.env_t
type type_info_t = env_t Type.info_t
type ctfe_val_t = type_info_t Ctfe_value.t
type ctx_t = (env_t, type_info_t, ctfe_val_t) Ctx.t
type type_gen_t = env_t Type.Generator.t


let rec code_generate ~bb node ctx =
  let open Ctx in
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

  | TAst.FunctionDefStmt (
        _, TAst.ParamsList (params), _, body, opt_attr, Some env
      ) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       (* *)
       let fenv_r = Env.FunctionOp.get_record env in
       let fenv_nr = Env.FunctionOp.get_normal_record env in

       let llparam_tys =
         fenv_r.Env.fn_param_types
         |> List.map (fun t -> lltype_of_typeinfo_ac ~bb:bb t ctx)
         |> Array.of_list
       in
       let llret_ty = lltype_of_typeinfo ~bb:bb fenv_r.Env.fn_return_type ctx in

       (* type of function *)
       let f_ty = L.function_type llret_ty llparam_tys in

       (* declare function *)
       let name = fenv_r.Env.fn_mangled |> Option.get in
       let f = L.declare_function name f_ty ctx.ir_module in
       Ctx.bind_env_to_val ctx env f;

       (* setup parameters *)
       let param_envs = fenv_nr.Env.fn_n_param_envs |> Array.of_list in
       let ll_params = L.params f in
       assert (Array.length ll_params = Array.length param_envs);
       let declare_param_var optenv llvar =
         match optenv with
         | Some env ->
            begin
              let venv = Env.VariableOp.get_record env in
              let var_name = venv.Env.var_name in
              L.set_value_name var_name llvar;
              Ctx.bind_env_to_val ctx env llvar
            end
         | None -> ()
       in
       Array.iter2 declare_param_var param_envs ll_params;

       (* entry block *)
       let bb = L.append_block ctx.ir_context "entry" f in
       L.position_at_end bb ctx.ir_builder;

       (**)
       code_generate body ctx ~bb:(Some bb);

       (**)
       ignore (L.build_ret_void ctx.ir_builder);

       Llvm_analysis.assert_valid_function f;

       Ctx.mark_env_as_defined ctx env
     end

  | TAst.ExternFunctionDefStmt (
        _, TAst.ParamsList (params), _, extern_fname, opt_attr, Some env
      ) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       let fr = Env.FunctionOp.get_extern_record env in

       match fr.Env.fn_e_is_builtin with
       | true -> () (* DO NOTHING *)
       | false ->
          begin
            (* *)
            let fenv_r = Env.FunctionOp.get_record env in

            let llparam_tys =
              fenv_r.Env.fn_param_types
              |> List.map (fun t -> lltype_of_typeinfo_ac ~bb:bb t ctx)
              |> Array.of_list
            in
            let llret_ty = lltype_of_typeinfo ~bb:bb fenv_r.Env.fn_return_type ctx in

            (* type of function *)
            let f_ty = L.function_type llret_ty llparam_tys in

            let llvm_func = L.declare_function extern_fname f_ty ctx.ir_module in

            Ctx.bind_env_to_val ctx env llvm_func;

            Ctx.mark_env_as_defined ctx env
          end
     end

  | TAst.ExternClassDefStmt (
        name, extern_cname, Some env
      ) ->
     begin
       if Ctx.is_env_defined ctx env then () else
       begin
         if Ctx.is_env_defined ctx env then ();

         let ty_gen = try Ctx.find_builtin_type ctx extern_cname with
                      | Not_found ->
                         failwith (Printf.sprintf "[ICE] builtin class \"%s\" is not found"
                                                  extern_cname)
         in
         let ty = match ty_gen with
           | Ctx.Const ty -> ty
           | Ctx.Gen f ->
              let cenv_r = Env.ClassOp.get_record env in
              f cenv_r.Env.cls_template_vals
         in
         Ctx.bind_env_to_type ctx env ty;

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

       let (llvm_val_raw, expr_ty) = code_generate_as_value ~bb:bb init_expr ctx in
       let llval = adjust_llval_form ~bb:bb var_type expr_ty llvm_val_raw ctx in

       L.set_value_name venv.Env.var_name llval;
       Ctx.bind_env_to_val ctx env llval;

       Ctx.mark_env_as_defined ctx env
     end

  | TAst.EmptyStmt -> ()

  | _ -> failwith "cannot generate : statement"

and code_generate_as_value ?(bb=None) node ctx : (L.llvalue * 'env Type.info_t)=
  let open Ctx in
  match node with
  | TAst.GenericCall (name, args, ret_ty, Some env) ->
     begin
       let f_er = Env.FunctionOp.get_record env in
       let {
         Env.fn_name = fn_name;
         Env.fn_detail = detail;
         Env.fn_param_types = param_tys;
       } = f_er in

       let (llvals, tys) = args
                           |> List.map (fun n -> code_generate_as_value ~bb:bb n ctx)
                           |> List.split
       in
       let conv_funcs =
         List.map2 (fun t s -> adjust_llval_form ~bb:bb t s) param_tys tys
       in
       (* TODO: conv *)
       let llargs = List.map2 (fun f v -> f v ctx) conv_funcs llvals
                    |> Array.of_list
       in

       let fn_s_name = Nodes.string_of_id_string fn_name in
       let llval = match detail with
         (* normal function *)
         | Env.FnRecordNormal _ ->
            begin
              Printf.printf "gen value: debug / function %s\n" fn_s_name;
              let f = find_val_from_env_with_force_generation ~bb:bb ctx env in
              L.build_call f llargs "" ctx.ir_builder
            end
         (* extern function *)
         | Env.FnRecordExtern {
               Env.fn_e_name = extern_name;
               Env.fn_e_is_builtin = is_builtin;
             } ->
            begin
              (* TODO: fix *)
              match is_builtin with
              | true ->
                 begin
                   Printf.printf "gen value: debug / builtin %s = \"%s\"\n"
                                 fn_s_name extern_name; flush_all ();

                   let builtin_gen_f = Ctx.find_builtin_func ctx extern_name in
                   builtin_gen_f llargs ctx
                 end
              | false ->
                 begin
                   Printf.printf "gen value: debug / extern  %s = \"%s\"\n"
                                 fn_s_name extern_name; flush_all ();
                   let extern_f =
                     find_val_from_env_with_force_generation ~bb:bb ctx env in
                   L.build_call extern_f llargs "" ctx.ir_builder
                 end
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

  | TAst.Id (name, Some rel_env) ->
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
            let llval = try Ctx.find_val_from_env ctx rel_env with
                        | Not_found -> failwith "[ICE] variable env is not found"
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
                                                (Type.UniqueTy rel_env)
                                                r.Env.cls_template_vals
                                                Type_attr.undef
                 in
                 let itype_id = Option.get ty.Type.ti_id in
                 Printf.printf "##### type_id = %s\n" (Int64.to_string itype_id);

                 (* return the internal typeid as a type *)
                 let llval = L.const_of_int64 (L.i64_type ctx.ir_context)
                                              itype_id
                                              Type.is_type_id_signed
                 in
                 let ty = tsets.Type_sets.ts_type_type in
                 (llval, ty)
               end
            | None ->
               begin
                 Env.print rel_env;
                 failwith @@ "[ICE] TAst.Id: class in RUNTIME"
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
                        Type.ti_id = opt_tid;
                        Type.ti_sort = ty_sort;
                      } = ty in
                      match ty_sort with
                      | Type.UniqueTy _
                      | Type.NotDetermined _ ->
                         begin
                           match opt_tid with
                           | Some tid ->
                              (* return the internal typeid as a type *)
                              let llval =
                                L.const_of_int64 (L.i64_type ctx.ir_context)
                                                 tid
                                                 Type.is_type_id_signed
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


and force_target_generation : 'a. ('ctx -> 'env -> 'a) -> 'ctx -> 'env -> 'bb -> 'a =
  fun f ctx env bb ->
  try f ctx env with
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
       try f ctx env with
       | Not_found ->
          begin
            Env.print env;
            failwith "[ICE] force_target_generation: couldn't find target"
          end
     end

and find_val_from_env_with_force_generation ~bb ctx env =
  force_target_generation Ctx.find_val_from_env
                          ctx env bb

and find_type_from_env_with_force_generation ~bb ctx env =
  force_target_generation Ctx.find_type_from_env
                          ctx env bb


and lltype_of_typeinfo ~bb ty ctx =
  let open Ctx in
  let cenv = Type.as_unique ty in
  let ll_ty = find_type_from_env_with_force_generation ~bb:bb ctx cenv in
  ll_ty

and lltype_of_typeinfo_ac ~bb ty ctx =
  let ll_ty = lltype_of_typeinfo ~bb:bb ty ctx in
  if is_address_representation ty then
    L.pointer_type ll_ty
  else
    ll_ty

and is_address_representation ty =
  let {
    Type_attr.ta_ref_val = rv;
    Type_attr.ta_mut = mut;
  } = ty.Type.ti_attr in
  match rv with
  | Type_attr.XRef -> false (* TODO: check whether object is heavy *)
  | Type_attr.Ref ->
     begin
       match mut with
       | Type_attr.Mutable -> true
       | Type_attr.Const
       | Type_attr.Immutable -> false    (* TODO: check whether object is heavy *)
       | _ -> failwith "[ICE] Unexpected : mut"
     end
  | Type_attr.Val ->
     begin
       match mut with
       | Type_attr.Mutable -> true
       | Type_attr.Const
       | Type_attr.Immutable -> false    (* TODO: check whether object is heavy *)
       | _ -> failwith "[ICE] Unexpected : mut"
     end
  | _ -> failwith "[ICE] Unexpected : rv"

and adjust_llval_form ~bb trg_ty src_ty llval ctx =
  let open Ctx in
  Printf.printf "%b, %b\n"
                (is_address_representation trg_ty)
                (is_address_representation src_ty);
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


let make_default_context ?(opt_type_sets=None) ?(opt_uni_map=None) () =
  let ir_context = L.global_context () in
  let ir_builder = L.builder ir_context in
  let ir_module = L.create_module ir_context "Rill" in

  Ctx.init
    ~ir_context:ir_context
    ~ir_builder:ir_builder
    ~ir_module:ir_module
    ~type_sets:opt_type_sets
    ~uni_map:opt_uni_map


let regenerate_module ctx =
  let ir_module = L.create_module ctx.Ctx.ir_context "Rill" in
  ctx.Ctx.ir_module <- ir_module


let inject_builtins ctx =
  let open Ctx in
  let register_builtin_type name ty =
    Ctx.bind_builtin_type ctx name ty;
    Printf.printf "debug / registerd builtin type = \"%s\"\n" name
  in
  let register_builtin_func name f =
    Ctx.bind_builtin_func ctx name f;
    Printf.printf "debug / registerd builtin func = \"%s\"\n" name
  in

  (* type is represented as int64 in this context.
   * It donates ID of type in the type generator
   *)
  begin
    let open Builtin_info in
    register_builtin_type type_type_i.internal_name
                          (Ctx.Const (L.i64_type ctx.ir_context));

    register_builtin_type void_type_i.internal_name
                          (Ctx.Const (L.void_type ctx.ir_context));
    register_builtin_type bool_type_i.internal_name
                          (Ctx.Const (L.i1_type ctx.ir_context));
    register_builtin_type int32_type_i.internal_name
                          (Ctx.Const (L.i32_type ctx.ir_context));

    register_builtin_type array_type_i.internal_name
                          (Ctx.Gen (
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

  let add_int_int args ctx =
    assert (Array.length args = 2);
    L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
  in
  register_builtin_func "__builtin_op_binary_+_int_int" add_int_int


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
