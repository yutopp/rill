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
  ctx : (Context.t[@printer fun fmt _ -> fprintf fmt ""]);
  module_name : string;
  mutable funcs_rev : func_assoc_t list;
}

and func_assoc_t = string * Func.t [@@deriving show]

let create ~ctx : t = { ctx; module_name = ""; funcs_rev = [] }

let append_func m name f = m.funcs_rev <- (name, f) :: m.funcs_rev

let funcs m : func_assoc_t list = List.rev m.funcs_rev
