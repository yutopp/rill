(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* TODO: fix *)
type t = {
  mutable funcs: (string * Func.t) list;
}
[@@deriving sexp_of]

let create () : t =
  {
    funcs = [];
  }

let append_func m name f =
  m.funcs <- (name, f) :: m.funcs

let funcs m =
  List.rev m.funcs
