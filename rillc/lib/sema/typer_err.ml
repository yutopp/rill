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

type t = { diff : diff_t; kind : kind_t; nest : t option }

and diff_t =
  | Type of { lhs : Typing.Type.t; rhs : Typing.Type.t }
  | Linkage of {
      lhs : Typing.Type.func_linkage_t;
      rhs : Typing.Type.func_linkage_t;
    }

and kind_t =
  | ErrFuncArgLength of { r : int; l : int }
  | ErrFuncArgs of int
  | ErrFuncArgRet
  | ErrFuncLinkage
  | ErrArrayElem
  | ErrArrayLength of { r : int; l : int }
  | ErrUnify
