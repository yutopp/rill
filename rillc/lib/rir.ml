(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  funcs: func_t list;
}

and func_t = {
  name: string;
  params: placeholder_t list;
  mutable ret_var: value_t option;
  mutable bb_index: bb_index_t;
  mutable bbs: bb_t Array.t;
}

and bb_t = {
  index: bb_index_t;
  mutable insts: inst_t Array.t;
  mutable terminator: terminator_t option;
}

and bb_index_t =
  int

and value_t = {kind: value_kind_t; ty: Hir.Ty.t}

and value_kind_t =
  | RVal of value_r_t
  | LVal of placeholder_t

and value_r_t =
  | ValueInt of int

and inst_t =
  | Copy of placeholder_t * value_t
  | Nop

and terminator_t =
  | Jump of bb_index_t
  | Cond of placeholder_t * bb_index_t * bb_index_t
  | Ret of placeholder_t

and placeholder_t = string * int option
[@@deriving sexp]

module Builder = struct
end

let generate hir =
  let k_norm = Rir_k_norm.generate hir in
  (*let _ = Stdio.printf "K = \n%s\n" (Rir_k_norm.sexp_of_t k_norm |> Sexp.to_string_hum ~indent:2) in*)
  {
    funcs = [];
  }
