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
  mutable bbs: (bb_index_t, bb_t) Hashtbl.t;
}

and bb_t = {
  bb_name: string;
  mutable bb_insts: inst_t Array.t;
  mutable bb_terminator: terminator_t option;
}

and bb_index_t = int

and value_t = {
  kind: value_kind_t;
  ty: Hir.Ty.t;
  span: Span.t;
}

and value_kind_t =
  | Call of placeholder_t * placeholder_t list
  | RVal of value_r_t
  | LVal of placeholder_t
  | Unit

and value_r_t =
  | ValueInt of int
  | ValueString of string

and inst_t =
  | Let of placeholder_t * value_t
  | Nop

and terminator_t =
  | Jump of bb_index_t
  | Cond of placeholder_t * bb_index_t * bb_index_t
  | Ret of placeholder_t

and placeholder_t = string
[@@deriving sexp_of]

module BB = struct
  type t = {
    name: string;
    mutable insts: inst_t list;
    mutable terminator: terminator_t option;
  }

  let create name =
    {
      name;
      insts = [];
      terminator = None;
    }

  let append bb inst =
    bb.insts <- inst :: bb.insts
end

module Func = struct
  type t = {
    name: string;
    params: placeholder_t list;
    mutable ret_var: value_t option;
    mutable bbs: (string, BB.t) Hashtbl.t;
  }

  let create name =
    {
      name;
      params = [];
      ret_var = None;
      bbs = Hashtbl.create (module String);
    }

  let insert_bb f bb =
    Hashtbl.add_exn f.bbs ~key:bb.BB.name ~data:bb
end

let make_module () =
  {
    funcs = [];
  }
