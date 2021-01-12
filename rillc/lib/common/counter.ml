(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = { c : int ref }

let create () = { c = ref 0 }

let count c = !(c.c)

let incr c = Int.incr c.c

let fresh c =
  let v = count c in
  incr c;
  v

let fresh_string c =
  let v = fresh c in
  Printf.sprintf "%d" v
