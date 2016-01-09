module L = Llvm
module LBW = Llvm_bitwriter

module CodeGeneratorType =
  struct
    type ir_context_t = L.llcontext
    type ir_builder_t = L.llbuilder
    type ir_module_t = L.llmodule
  end

module Ctx =
  struct
    include Codegen.Context.Make(CodeGeneratorType)
  end
open Ctx

type ctx_t = Ctx.t

let rec code_generate ctx node =
  match node with
  | Sema.TaggedAst.Module (inner, _) ->
     begin
       code_generate ctx inner
     end

  | Sema.TaggedAst.StatementList (nodes) ->
     let f nctx node = code_generate nctx node in
     List.fold_left f ctx nodes

  | Sema.TaggedAst.FunctionDefStmt (name, params, body, env) ->
     begin
       let i32_ty = L.i32_type ctx.ir_context in
       let void_ty = L.void_type ctx.ir_context in

       (* int -> void *)
       let f_ty = L.function_type void_ty [|i32_ty|] in
       let f = L.declare_function name f_ty ctx.ir_module in

       (* entry block *)
       let bb = L.append_block ctx.ir_context "entry" f in
       let f_begin_ip = L.instr_begin bb in
       L.position_builder f_begin_ip ctx.ir_builder;
       ignore (L.build_ret_void ctx.ir_builder);

       Llvm_analysis.assert_valid_function f;

       ctx
     end

  | _ -> failwith "cannot generate"


let generate node =
  let ir_context = L.global_context () in
  let ir_builder = L.builder ir_context in
  let ir_module = L.create_module ir_context "Rill" in

  let context = Ctx.init
                  ~ir_context:ir_context
                  ~ir_builder:ir_builder
                  ~ir_module:ir_module
  in
  code_generate context node


exception FailedToWriteBitcode
exception FailedToBuildBitcode
exception FailedToBuildExecutable


let create_executable ctx lib_name out_name =
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
