(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Base
open Stdint
module L = Llvm

type t = {
  memset_i32 : L.llvalue -> int8 ->
               Stdint.uint32 -> Stdint.uint32 -> bool -> L.llbuilder -> L.llvalue;
  memcpy_i32 : L.llvalue -> L.llvalue ->
               Stdint.uint32 -> Stdint.uint32 -> bool -> L.llbuilder -> L.llvalue;
}

let declare_intrinsics llctx llmod =
  (* http://llvm.org/docs/LangRef.html#llvm-memset-intrinsics *)
  let memset_i32 dest value len align is_volatile builder =
    let llmemset_i32 =
      let f = L.function_type (L.void_type llctx)
                              [|L.pointer_type (L.i8_type llctx);
                                (L.i8_type llctx);
                                (L.i32_type llctx);
                                (L.i32_type llctx);
                                (L.i1_type llctx);
                               |] in
      L.declare_function "llvm.memset.p0i8.i32" f llmod
    in

    let dest_p = L.build_bitcast dest
                                 (L.pointer_type (L.i8_type llctx))
                                 ""
                                 builder
    in
    let llval = L.const_int_of_string (L.i8_type llctx)
                                      (Stdint.Int8.to_string value) 10
    in
    let lllen = L.const_int_of_string (L.i32_type llctx)
                                      (Stdint.Uint32.to_string len) 10
    in
    let llalign = L.const_int_of_string (L.i32_type llctx)
                                        (Stdint.Uint32.to_string align) 10
    in
    L.build_call llmemset_i32 [|dest_p;
                                llval;
                                lllen;
                                llalign;
                                L.const_int (L.i1_type llctx) (Bool.to_int is_volatile);
                               |]
                 ""
                 builder;
  in

  (* http://llvm.org/docs/LangRef.html#llvm-memcpy-intrinsic *)
  let memcpy_i32 dest src len align is_volatile builder =
    let llmemcpy_i32 =
      let f = L.function_type (L.void_type llctx)
                              [|L.pointer_type (L.i8_type llctx);
                                L.pointer_type (L.i8_type llctx);
                                (L.i32_type llctx);
                                (L.i32_type llctx);
                                (L.i1_type llctx);
                               |] in
      L.declare_function "llvm.memcpy.p0i8.p0i8.i32" f llmod
    in

    let dest_p = L.build_bitcast dest
                                 (L.pointer_type (L.i8_type llctx))
                                 ""
                                 builder
    in
    let src_p = L.build_bitcast src
                                (L.pointer_type (L.i8_type llctx))
                                ""
                                builder
    in
    let lllen = L.const_int_of_string (L.i32_type llctx)
                                      (Stdint.Uint32.to_string len) 10
    in
    let llalign = L.const_int_of_string (L.i32_type llctx)
                                        (Stdint.Uint32.to_string align) 10
    in
    L.build_call llmemcpy_i32 [|dest_p;
                                src_p;
                                lllen;
                                llalign;
                                L.const_int (L.i1_type llctx) (Bool.to_int is_volatile);
                               |]
                 ""
                 builder;
  in

  {
    memset_i32 = memset_i32;
    memcpy_i32 = memcpy_i32;
  }
