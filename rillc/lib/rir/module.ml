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
  mutable funcs: Term.Func.t list;
}
[@@deriving sexp_of]

let create () : t =
  {
    funcs = [];
  }

let append_func m f =
  m.funcs <- f :: m.funcs
