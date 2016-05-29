(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
module L = Llvm

type t = {
  memcpy_i32 : L.llvalue -> L.llvalue ->
               Stdint.uint32 -> Stdint.uint32 -> bool -> L.llbuilder -> L.llvalue
}

let declare_intrinsics llctx llmod =
  let memcpy_i32 dest src len align is_volatile builder =
    (* http://llvm.org/docs/LangRef.html#llvm-memcpy-intrinsic *)
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
    memcpy_i32 = memcpy_i32;
  }
