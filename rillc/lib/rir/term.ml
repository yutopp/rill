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
module Type = Typing.Type

type t = {
  kind : value_kind_t;
  ty : (Type.t[@printer fun fmt _ -> fprintf fmt ""]);
  span : (Span.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and value_kind_t =
  | Call of placeholder_t * placeholder_t list
  | RVal of value_r_t
  | LVal of placeholder_t
  | Undef

and value_r_t =
  | ValueBool of bool
  | ValueInt of int
  | ValueString of string
  | ValueUnit

and inst_t = Let of placeholder_t * t | Assign of placeholder_t * string | Nop

and terminator_t =
  | Jump of string
  | Cond of placeholder_t * string * string
  | Ret of placeholder_t
  | RetVoid

and placeholder_t = string [@@deriving show]

module BB = struct
  type t = {
    name : string;
    mutable insts_rev : inst_t list;
    mutable terminator : terminator_t option;
  }
  [@@deriving show]

  let create name = { name; insts_rev = []; terminator = None }

  let append_inst bb inst = bb.insts_rev <- inst :: bb.insts_rev

  let get_insts bb = List.rev bb.insts_rev

  let set_terminator bb term =
    assert (Option.is_none bb.terminator);
    bb.terminator <- Some term

  let get_terminator_opt bb = bb.terminator
end
