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
  output : string;
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
    Os.ar' ~spec:target_spec ~objs:opts.input_files ~out:opts.output ()
  in
  let%bind () = Os.ranlib ~spec:target_spec ~out:opts.output () in

  Ok ()
