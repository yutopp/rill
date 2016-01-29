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
  end

module Ctx = Codegen.Context.Make(CodeGeneratorType)
type ctx_t = Env.id_t Ctx.t

let rec code_generate node ctx =
  let open Ctx in
  let module TAst = Sema.TaggedAst in
  match node with
  | TAst.Module (inner, _) ->
     begin
       code_generate inner ctx
     end

  | TAst.StatementList (nodes) ->
     let f nctx node = code_generate node nctx in
     List.fold_left f ctx nodes

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
       let nctx = code_generate body ctx in

       (**)
       ignore (L.build_ret_void ctx.ir_builder);

       Llvm_analysis.assert_valid_function f;

       nctx
     end

  | TAst.ExternFunctionDefStmt (name, TAst.ParamsList (params), _, extern_fname, Some env) ->
     begin
       let i32_ty = L.i32_type ctx.ir_context in
       let void_ty = L.void_type ctx.ir_context in

       (* int -> void *)
       let f_ty = L.function_type void_ty [|i32_ty|] in

       let llvm_func = L.declare_function extern_fname f_ty ctx.ir_module in

       Ctx.bind_env_to_val ctx env llvm_func;
       ctx
     end

  | TAst.VariableDefStmt (rv, TAst.VarInit (var_init), Some env) ->
     begin
       let (var_name, (_, opt_init_expr)) = var_init in
       let init_expr = Option.get opt_init_expr in
       let llvm_val = code_generate_as_value init_expr ctx in
       Ctx.bind_env_to_val ctx env llvm_val;

       L.set_value_name var_name llvm_val;

       ctx
     end

  | TAst.EmptyStmt -> ctx

  | _ -> failwith "cannot generate : statement"

and code_generate_as_value node ctx =
  let open Ctx in
  let module TAst = Sema.TaggedAst in
  match node with
  | TAst.BinaryOpExpr (lhs, op, rhs) ->
     begin
       failwith ""
     end
  | TAst.Int32Lit (v) ->
     begin
       L.const_int (L.i32_type ctx.ir_context) v
     end
  | _ -> failwith "cannot generate : as value"


let generate node =
  let ir_context = L.global_context () in
  let ir_builder = L.builder ir_context in
  let ir_module = L.create_module ir_context "Rill" in

  let context = Ctx.init
                  ~ir_context:ir_context
                  ~ir_builder:ir_builder
                  ~ir_module:ir_module
  in
  let ret_ctx = code_generate node context in
  L.dump_module ret_ctx.Ctx.ir_module;
  ret_ctx


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
  (*let sc = Sys.command (Printf.sprintf "g++ %s %s -o %s" (Filename.quote bin_name) (Filename.quote lib_name) (Filename.quote out_name)) in*)
  let sc = Sys.command (Printf.sprintf "gcc %s -o %s" (Filename.quote bin_name) (Filename.quote out_name)) in
  if sc <> 0 then raise FailedToBuildExecutable;
  ()
