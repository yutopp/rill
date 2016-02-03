module L = Llvm
module LE = Llvm_executionengine

type t = {
  cg_ctx        : Llvm_codegen.ctx_t;
  exec_engine   : LE.llexecutionengine;
}


let initialize () =
  if not (LE.initialize ()) then
    failwith "[ICE] Couldn't initialize LLVM backend";

  let module CgCtx = Llvm_codegen.Ctx in
  let codegen_ctx = Llvm_codegen.make_default_context () in
  let llmod = codegen_ctx.CgCtx.ir_module in

  let jit_engine = LE.create llmod in
  {
    cg_ctx = codegen_ctx;
    exec_engine = jit_engine;
  }

let tmp_expr_fname = "__rill_jit_tmp_expr"

let ctype_of_semantic_type ty =
  Ctypes.int

let execute engine node expr_ty =
  let module CgCtx = Llvm_codegen.Ctx in
  Printf.printf "JIT ==== execute!!!\n";

  (* save the module which has previous definitions to JIT engine *)
  LE.add_module engine.cg_ctx.CgCtx.ir_module engine.exec_engine;

  (* generate a new module to define a temporary function for CTFE *)
  Llvm_codegen.regenerate_module engine.cg_ctx;

  (* alias *)
  let ir_ctx = engine.cg_ctx.CgCtx.ir_context in
  let ir_mod = engine.cg_ctx.CgCtx.ir_module in
  let ir_builder = engine.cg_ctx.CgCtx.ir_builder in

  (**)
  LE.add_module ir_mod engine.exec_engine;

  let expr_llty = Llvm_codegen.lltype_of_typeinfo ~bb:None expr_ty engine.cg_ctx in

  (* unit -> (typeof expr) *)
  let f_ty = L.function_type expr_llty [||] in
  let f = L.declare_function tmp_expr_fname f_ty ir_mod in

  let bb = L.append_block ir_ctx "entry" f in
  L.position_at_end bb ir_builder;

  let llret = L.const_int (L.i32_type ir_ctx) 10 in

  ignore @@ L.build_ret llret ir_builder;

  Llvm_analysis.assert_valid_function f;

  (**)
  let open Ctypes in
  let cfunc_ty = Ctypes.void @-> returning (ctype_of_semantic_type expr_ty) in
  let jit_f =
    LE.get_function_address tmp_expr_fname (Foreign.funptr cfunc_ty) engine.exec_engine in

  let tv = jit_f () in
  Printf.printf "=========> %d\n" tv;

  (* dispose the module for this tmporary function
   * and regenerate a new module *)
  LE.remove_module ir_mod engine.exec_engine;
  Llvm_codegen.regenerate_module engine.cg_ctx;
  ()
