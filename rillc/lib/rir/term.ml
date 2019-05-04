(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics

type bb_index_t = int

and t = {
  kind: value_kind_t;
  ty: Type.t;
  span: Span.t sexp_opaque;
}

and value_kind_t =
  | Call of placeholder_t * placeholder_t list
  | RVal of value_r_t
  | LVal of placeholder_t
  | Undef

and value_r_t =
  | ValueInt of int
  | ValueString of string
  | ValueUnit

and inst_t =
  | Let of placeholder_t * t
  | Nop

and terminator_t =
  | Jump of bb_index_t
  | Cond of placeholder_t * bb_index_t * bb_index_t
  | Ret of placeholder_t
  | RetVoid

and placeholder_t = string
[@@deriving sexp_of]

module BB = struct
  type t = {
    name: string;
    mutable insts: inst_t list;
    mutable terminator: terminator_t option;
  }
  [@@deriving sexp_of]

  let create name =
    {
      name;
      insts = [];
      terminator = None;
    }

  let append_inst bb inst =
    bb.insts <- inst :: bb.insts

  let get_insts bb =
    List.rev bb.insts

  let set_terminator bb term =
    assert(Option.is_none bb.terminator);
    bb.terminator <- Some term

  let get_terminator_opt bb =
    bb.terminator
end
