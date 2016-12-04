(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Type_sets
open Ctfe_value

module L = Llvm
module LE = Llvm_executionengine

type t = {
  cg_ctx        : Codegen_llvm.ctx_t;
  exec_engine   : LE.llexecutionengine;
}


module JITCounter =
  struct
    let counter = ref Int64.zero

    let generate_fresh_name () =
      let c = !counter in
      if c = Int64.max_int then
        failwith "[ICE] Internal JIT cache id is reached to max id...";
      counter := Int64.succ !counter;

      "__rill_jit_tmp_expr_" ^ (Int64.to_string c)
  end


let initialize type_sets uni_map =
  if not (LE.initialize ()) then
    failwith "[ICE] Couldn't initialize LLVM backend";

  let module CgCtx = Codegen_llvm.Ctx in
  let codegen_ctx =
    Codegen_llvm.make_default_context ~type_sets:type_sets
                                      ~uni_map:uni_map
  in
  Codegen_llvm.inject_builtins codegen_ctx;

  let llmod = codegen_ctx.CgCtx.ir_module in
  let jit_engine = LE.create llmod in
  {
    cg_ctx = codegen_ctx;
    exec_engine = jit_engine;
  }


let invoke_function engine fname ret_ty type_sets =
  let open Ctypes in
  let call_by_type fn_ty =
    let jit_f =
         LE.get_function_address fname (Foreign.funptr fn_ty)
                                 engine.exec_engine
    in
    jit_f ()
  in

  match ret_ty with
  | ty when Type.has_same_class ty type_sets.ts_type_type ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.int64_t in
       let ret_val = call_by_type cfunc_ty in

       let ty =
         Type.Generator.find_type_by_cache_id type_sets.ts_type_gen ret_val in
       Ctfe_value.Type ty
     end

  | ty when Type.has_same_class ty !(type_sets.ts_bool_type_holder) ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.bool in
       let ret_val = call_by_type cfunc_ty in

       Ctfe_value.Bool ret_val
     end

  | ty when Type.has_same_class ty !(type_sets.ts_int32_type_holder) ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.int32_t in
       let ret_val = call_by_type cfunc_ty in

       Ctfe_value.Int32 ret_val
     end

  | ty when Type.has_same_class ty !(type_sets.ts_uint32_type_holder) ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.uint32_t in
       let ret_val = call_by_type cfunc_ty in

       Ctfe_value.Uint32 (Stdint.Uint32.of_int32 (Unsigned.UInt32.to_int32 ret_val))
     end

  | _ ->
     begin
       failwith "[ICE] this type is not supported"
     end


let execute engine expr_node expr_ty type_sets =
  let module CgCtx = Codegen_llvm.Ctx in
  (* TODO: add cache... *)
  (*Debug.printf "JIT ==== execute!!!\n";*)

  (* save the module which has previous definitions to JIT engine *)
  LE.add_module engine.cg_ctx.CgCtx.ir_module engine.exec_engine;

  (* generate a new module to define a temporary function for CTFE *)
  (*Codegen_llvm.regenerate_module engine.cg_ctx;*)

  (* alias *)
  let ir_ctx = engine.cg_ctx.CgCtx.ir_context in
  let ir_mod = engine.cg_ctx.CgCtx.ir_module in
  let ir_builder = engine.cg_ctx.CgCtx.ir_builder in

  (**)
  let expr_llty = Codegen_llvm.lltype_of_typeinfo expr_ty engine.cg_ctx in

  (**)
  let tmp_expr_fname = JITCounter.generate_fresh_name () in

  (* declare temporary funtion : unit -> (typeof expr) *)
  let f_ty = L.function_type expr_llty [||] in
  let f = L.declare_function tmp_expr_fname f_ty ir_mod in

  let bb = L.append_block ir_ctx "entry" f in
  L.position_at_end bb ir_builder;

  (* generate a LLVM value from the expression *)
  try
    begin
      let (expr_llval, _, is_addr) =
        Codegen_llvm.generate_code expr_node engine.cg_ctx in
      Debug.printf ">>> DUMP LLVM VALUE / is_addr: %b\n" is_addr;
      Codegen_llvm.debug_dump_value expr_llval;
      Debug.printf "===\n";
      let expr_llval = match is_addr with
        | true -> L.build_load expr_llval "" ir_builder
        | faise -> expr_llval
      in
      Codegen_llvm.debug_dump_value expr_llval;
      Debug.printf "<<< DUMP LLVM VALUE\n";
      ignore @@ L.build_ret expr_llval ir_builder;

      Llvm_analysis.assert_valid_function f;
      Codegen_llvm.debug_dump_value f;

      (**)
      LE.add_module ir_mod engine.exec_engine;

      (**)
      let ctfe_val = invoke_function engine tmp_expr_fname expr_ty type_sets in

      (* Remove the module for this tmporary function from execution engine.
       * However, the module will be used until next time.
       *)
      LE.remove_module ir_mod engine.exec_engine;
      (* Codegen_llvm.debug_dump_module engine.cg_ctx.CgCtx.ir_module; *)

      L.delete_function f;

      (ctfe_val, is_addr)
    end
  with
  | Ctfe_exn.Meta_var_un_evaluatable uni_id ->
     begin
       L.delete_function f;
       (Ctfe_value.Undef uni_id, false)
     end


let register_metavar engine ctfe_val is_addr env =
  Codegen_llvm.register_metaval ctfe_val is_addr env engine.cg_ctx
