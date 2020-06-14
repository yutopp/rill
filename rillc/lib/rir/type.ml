(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = unit [@@deriving show]

let create () = ()

let to_string ~indent name r_ty =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (String.make indent ' ');

  Buffer.add_string buf (Printf.sprintf "Type: name = %s\n" name);

  Buffer.contents buf
