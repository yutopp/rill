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
module Ctx = Codegen.Context.Make(CodeGeneratorType)
module TAst = Codegen.TAst

type env_t = TAst.t Env.env_t
type type_info_t = env_t Type.info_t
type ctx_t = (env_t, type_info_t)  Ctx.t
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
        name, TAst.ParamsList (params), _, body, opt_attr, Some env
      ) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       (* *)
       let fenv_r = Env.FunctionOp.get_record env in
       let fenv_nr = Env.FunctionOp.get_normal_record env in

       let llparam_tys =
         fenv_r.Env.fn_param_types
         |> List.map (fun t -> lltype_of_typeinfo ~bb:bb t ctx)
         |> Array.of_list
       in
       let llret_ty = lltype_of_typeinfo ~bb:bb fenv_r.Env.fn_return_type ctx in

       (* type of function *)
       let f_ty = L.function_type llret_ty llparam_tys in

       (* declare function *)
       let name = Nodes.string_of_id_string name in (*TODO: use the value in the env*)
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
        name, TAst.ParamsList (params), _, extern_fname, opt_attr, Some env
      ) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       let fr = Env.FunctionOp.get_extern_record env in

       match fr.Env.fn_e_is_builtin with
       | true -> () (* DO NOTHING *)
       | false ->
          begin
            (* TODO: fix fix fix fix *)
            let i32_ty = L.i32_type ctx.ir_context in
            let void_ty = L.void_type ctx.ir_context in

            (* int -> void *)
            let f_ty = L.function_type void_ty [|i32_ty|] in

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

         let ty = try Ctx.find_builtin_type ctx extern_cname with
                  | Not_found ->
                     failwith (Printf.sprintf "[ICE] builtin class \"%s\" is not found"
                                              extern_cname)
         in
         Ctx.bind_env_to_type ctx env ty;

         Ctx.mark_env_as_defined ctx env
       end
     end

  | TAst.VariableDefStmt (rv, TAst.VarInit (var_init), Some env) ->
     if Ctx.is_env_defined ctx env then () else
     begin
       if Ctx.is_env_defined ctx env then ();

       let (var_name, (_, opt_init_expr)) = var_init in
       let init_expr = Option.get opt_init_expr in
       let llvm_val = code_generate_as_value ~bb:bb init_expr ctx in
       Ctx.bind_env_to_val ctx env llvm_val;

       L.set_value_name var_name llvm_val;

       Ctx.mark_env_as_defined ctx env
     end

  | TAst.EmptyStmt -> ()

  | _ -> failwith "cannot generate : statement"

and code_generate_as_value ?(bb=None) node ctx =
  let open Ctx in
  match node with
  | TAst.GenericCall (name, args, Some env) ->
     begin
       let llargs = args
                    |> List.map (fun n -> code_generate_as_value ~bb:bb n ctx)
                    |> Array.of_list in
       let { Env.er = er; _ } = env in
       let llval = match er with
         | Env.Function (_, r) ->
            begin
              let {
                Env.fn_name = fn_name;
                Env.fn_detail = detail;
                _
              } = r in
              let fn_s_name = Nodes.string_of_id_string fn_name in
              match detail with
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
            end
         | _ -> failwith @@ "[ICE] codegen; function lhs id " ^ name
       in
       llval
     end

  | TAst.Int32Lit (v) ->
     begin
       L.const_int (L.i32_type ctx.ir_context) v
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
            try Ctx.find_val_from_env ctx rel_env with
            | Not_found -> failwith "[ICE] variable env is not found"
          end

       | Env.Class (_, r) ->
          begin
            match ctx.type_generator with
            | Some gen ->
               begin
                 let (_, itype_id) =
                   Type.Generator.generate_type gen (Type.UniqueTy
                                                       {
                                                         Type.ty_cenv = rel_env;
                                                       })
                 in
                 Printf.printf "##### type_id = %s\n" (Int64.to_string itype_id);

                 (* return the internal typeid as a type *)
                 L.const_of_int64 (L.i64_type ctx.ir_context)
                                  itype_id
                                  Type.is_type_id_signed
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
            match ctx.uni_map with
            | Some uni_map ->
               begin
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
                             _
                           } = ty in
                           match ty_sort with
                           | Type.UniqueTy _
                           | Type.NotDetermined _ ->
                              begin
                                match opt_tid with
                                | Some tid ->
                                   (* return the internal typeid as a type *)
                                   L.const_of_int64 (L.i64_type ctx.ir_context)
                                                    tid
                                                    Type.is_type_id_signed
                                | None -> failwith "[ICE] codegen_llvm : nontermin"
                              end
                           | _-> failwith "[ICE] codegen_llvm : type"
                         end
                    end
                 | _ -> raise Ctfe_exn.Meta_var_un_evaluatable
               end
            | None ->
               begin
                 Env.print rel_env;
                 failwith "[ICE] TAst.Id: meta variable in RUNTIME"
               end
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
  let {
    Type.ty_cenv = cenv;
    _
  } = Type.as_unique ty in
  let ll_ty = find_type_from_env_with_force_generation ~bb:bb ctx cenv in
  ll_ty



let make_default_context ?(opt_type_gen=None) ?(opt_uni_map=None) () =
  let ir_context = L.global_context () in
  let ir_builder = L.builder ir_context in
  let ir_module = L.create_module ir_context "Rill" in

  Ctx.init
    ~ir_context:ir_context
    ~ir_builder:ir_builder
    ~ir_module:ir_module
    ~type_generator:opt_type_gen
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
  register_builtin_type "__builtin_type_type" (L.i64_type ctx.ir_context);

  register_builtin_type "__builtin_type_void" (L.void_type ctx.ir_context);
  register_builtin_type "__builtin_type_int" (L.i32_type ctx.ir_context);


  let add_int_int args ctx =
    assert (Array.length args = 2);
    L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
  in
  register_builtin_func "__builtin_op_binary_+_int_int" add_int_int


let generate node =
  let ctx = make_default_context () in
  inject_builtins ctx;

  code_generate ~bb:None node ctx;
  L.dump_module ctx.Ctx.ir_module;
  ctx


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
