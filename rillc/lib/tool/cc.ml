(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Triple = Common.Triple
module Os = Common.Os

type t = {
  sysroot : string option;
  target : Triple.tag_t option;
  output : string option;
  only_pp : bool;
  only_comp : bool;
  only_comp_asm : bool;
  c_linker_flags : string list;
  input_files : string list;
}

let entry opts =
  let open Result.Let_syntax in
  let sysroot = Args.sysroot opts.sysroot in

  let host_triple = Args.host_triple () in
  let target_triple = Args.target_triple opts.target in

  let target_sysroot = Args.target_sysroot ~sysroot ~triple:target_triple in

  let%bind target_spec = Args.target_spec ~target_sysroot in

  let%bind () =
    let linker_flags = opts.c_linker_flags in
    let lib_dirs = (* TODO *) [] in
    let lib_names = (* TODO *) [] in
    let objs = (* TODO *) opts.input_files in
    let out = opts.output |> Option.value ~default:"a.out" in
    Os.cc_exe ~spec:target_spec ~only_pp:opts.only_pp ~only_comp:opts.only_comp
      ~only_comp_asm:opts.only_comp_asm ~linker_flags ~lib_dirs ~lib_names ~objs
      ~out ()
  in

  Ok ()
