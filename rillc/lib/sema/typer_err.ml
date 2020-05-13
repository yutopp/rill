(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module IntMap = Map.M (Int)

type t = {
  lhs : Typing.Type.t;
  rhs : Typing.Type.t;
  kind : kind_t;
  nest : t option;
}

and kind_t =
  | ErrFuncArgLength of { r : int; l : int }
  | ErrFuncArgs of int
  | ErrFuncArgRet
  | ErrUnify
