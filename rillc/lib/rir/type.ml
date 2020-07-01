(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = { name : Common.Chain.Nest.t; inner_ty : Typing.Type.t }
[@@deriving show]

let create ~name ~inner_ty = { name; inner_ty }

let to_string ~indent r_ty =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (String.make indent ' ');

  let { name; _ } = r_ty in
  Buffer.add_string buf
    (Printf.sprintf "Type: name = %s\n" (Common.Chain.Nest.show name));

  Buffer.contents buf
