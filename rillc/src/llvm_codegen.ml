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
type ctx_t = Env.id_t Ctx.t


let rec code_generate ?(ip=None) node ctx =
  let open Ctx in
  let module TAst = Sema.TaggedAst in
  match node with
  | TAst.Module (inner, _) ->
     begin
       code_generate inner ctx
     end

  | TAst.StatementList (nodes) ->
     nodes |> List.iter @@ fun n -> code_generate n ctx

  | TAst.FunctionDefStmt (name, TAst.ParamsList (params), _, body, Some env) ->
     begin
       let i32_ty = L.i32_type ctx.ir_context in
       let void_ty = L.void_type ctx.ir_context in

       (* int -> void *)
       let name = Nodes.string_of_id_string name in (*TODO: use the value in the env*)
       let f_ty = L.function_type void_ty [|i32_ty|] in
       let f = L.declare_function name f_ty ctx.ir_module in

       (* entry block *)
       let bb = L.append_block ctx.ir_context "entry" f in
       let f_begin_ip = L.instr_begin bb in
       L.position_builder f_begin_ip ctx.ir_builder;

       (**)
       code_generate body ctx ~ip:(Some f_begin_ip);

       (**)
       ignore (L.build_ret_void ctx.ir_builder);

       Llvm_analysis.assert_valid_function f;

       ()
     end

  | TAst.ExternFunctionDefStmt
      (name, TAst.ParamsList (params), _, extern_fname, Some env) ->
     begin
       let fr = Env.FunctionOp.get_extern_record env in

       match fr.Env.fn_e_is_builtin with
       | true -> () (* DO NOTHING *)
       | false ->
          begin
            let i32_ty = L.i32_type ctx.ir_context in
            let void_ty = L.void_type ctx.ir_context in

            (* int -> void *)
            let f_ty = L.function_type void_ty [|i32_ty|] in

            let llvm_func = L.declare_function extern_fname f_ty ctx.ir_module in

            Ctx.bind_env_to_val ctx env llvm_func;
            ()
          end
     end

  | TAst.VariableDefStmt (rv, TAst.VarInit (var_init), Some env) ->
     begin
       let (var_name, (_, opt_init_expr)) = var_init in
       let init_expr = Option.get opt_init_expr in
       let llvm_val = code_generate_as_value init_expr ctx ~ip:ip in
       Ctx.bind_env_to_val ctx env llvm_val;

       L.set_value_name var_name llvm_val;

       ()
     end

  | TAst.ExprStmt e ->
     begin
       ignore @@ code_generate_as_value e ctx;
       ()
     end

  | TAst.EmptyStmt -> ()

  | _ -> failwith "cannot generate : statement"

and code_generate_as_value ?(ip=None) node ctx =
  let open Ctx in
  let module TAst = Sema.TaggedAst in
  match node with
  | TAst.GenericCall (name, args, Some env) ->
     begin
       let llargs = args
                    |> List.map (fun n -> code_generate_as_value n ctx)
                    |> Array.of_list in
       let { Env.er = er; _ } = env in
       let llval = match er with
         | Env.Function (_, r) ->
            begin
              let { Env.fn_detail = detail; _ } = r in
              match detail with
              | Env.FnRecordExtern {
                  Env.fn_e_name = extern_name;
                  Env.fn_e_is_builtin = is_builtin;
                } ->
                 begin
                   (* TODO: fix *)
                   match is_builtin with
                   | true ->
                      begin
                        Printf.printf "debug / builtin = \"%s\"\n" extern_name;

                        let builtin_gen_f = Ctx.find_builtin_func ctx extern_name in
                        builtin_gen_f llargs ctx
                      end
                   | false ->
                      begin
                        Printf.printf "debug / extern = \"%s\"\n" extern_name;
                        let extern_f =
                          find_val_from_env_with_force_generation ctx env ip in
                        L.build_call extern_f llargs "" ctx.ir_builder
                      end
                 end
              | Env.FnRecordNormal _ -> failwith "codegen: not implemented fun"
              | _ -> failwith "codegen: invalid function record"
            end
         | _ -> failwith @@ "codegen; id " ^ name
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
       | Env.Function (_, _) ->
          begin
            (* *)
            failwith "function"
          end
       | Env.Variable (vr) ->
          begin
            Ctx.find_val_from_env ctx rel_env
          end
       | _ -> failwith @@ "codegen; id " ^ (Nodes.string_of_id_string name)
     end

  | _ -> failwith "cannot generate : as value"


and code_generate_by_interrupt node ctx opt_ip =
  code_generate node ctx;
  (* Restore position *)
  opt_ip |> Option.may (fun ip -> L.position_builder ip ctx.Ctx.ir_builder)


and find_val_from_env_with_force_generation ctx env opt_ip =
  try Ctx.find_val_from_env ctx env with
  | Not_found ->
     begin
       let { Env.rel_node = rel_node; _ } = env in
       let node = match rel_node with
         | Some v -> v
         | None ->
            failwith "find_val_from_env_with_force_generation: there is no rel node"
       in
       code_generate_by_interrupt node ctx opt_ip;

       (* retry *)
       try Ctx.find_val_from_env ctx env with
       | Not_found ->
          failwith "find_val_from_env_with_force_generation: couldn't find value"
     end


let make_default_context () =
  let ir_context = L.global_context () in
  let ir_builder = L.builder ir_context in
  let ir_module = L.create_module ir_context "Rill" in

  Ctx.init
    ~ir_context:ir_context
    ~ir_builder:ir_builder
    ~ir_module:ir_module


let inject_builtins ctx =
  let open Ctx in
  let register_builtin name f =
    Ctx.bind_builtin_func ctx name f;
    Printf.printf "debug / registerd builtin = \"%s\"\n" name
  in

  let add_int_int args ctx =
    assert (Array.length args = 2);
    L.build_add args.(0) args.(1) "" ctx.Ctx.ir_builder
  in
  register_builtin "__builtin_op_binary_+_int_int" add_int_int


let generate node =
  let ctx = make_default_context () in
  inject_builtins ctx;

  code_generate node ctx;
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
