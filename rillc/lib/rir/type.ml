(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = { name : Typing.Type.t Path.t; ty_sc : Typing.Scheme.t }
[@@deriving show]

let create ~name ~ty_sc = { name; ty_sc }

let to_string ~indent r_ty =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (String.make indent ' ');

  let { name; ty_sc } = r_ty in
  Buffer.add_string buf
    (Printf.sprintf "Type: name = '%s' :: %s\n"
       (Path.to_string ~to_s:Typing.Type.to_string name)
       (Typing.Scheme.to_string ty_sc));

  Buffer.contents buf
