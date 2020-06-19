(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module L = Llvm

(* http://llvm.org/docs/LangRef.html#llvm-memcpy-intrinsic *)

type t = {
  memcpy_i32 :
    L.llbuilder -> L.llvalue -> L.llvalue -> int -> int -> bool -> L.llvalue;
}

let gen_memcpy_i32 ll_ctx ll_mod =
  let llmemcpy_i32 =
    let ty =
      L.function_type (L.void_type ll_ctx)
        [|
          L.pointer_type (L.i8_type ll_ctx);
          L.pointer_type (L.i8_type ll_ctx);
          L.i32_type ll_ctx;
          L.i32_type ll_ctx;
          L.i1_type ll_ctx;
        |]
    in
    L.declare_function "llvm.memcpy.p0i8.p0i8.i32" ty ll_mod
  in
  let payload ll_builder dest src len align is_volatile =
    let dest_p =
      L.build_bitcast dest (L.pointer_type (L.i8_type ll_ctx)) "" ll_builder
    in
    let src_p =
      L.build_bitcast src (L.pointer_type (L.i8_type ll_ctx)) "" ll_builder
    in
    let lllen =
      let s = Stdint.Int32.of_int len |> Stdint.Int32.to_string in
      L.const_int_of_string (L.i32_type ll_ctx) s 10
    in
    let llalign =
      let s = Stdint.Int32.of_int align |> Stdint.Int32.to_string in
      L.const_int_of_string (L.i32_type ll_ctx) s 10
    in
    L.build_call llmemcpy_i32
      [|
        dest_p;
        src_p;
        lllen;
        llalign;
        L.const_int (L.i1_type ll_ctx) (Bool.to_int is_volatile);
      |]
      "" ll_builder
  in
  payload

let load_intrinsics ll_ctx ll_mod =
  let memcpy_i32 = gen_memcpy_i32 ll_ctx ll_mod in
  { memcpy_i32 }
