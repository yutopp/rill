(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module ArgLocMap = Map.Make(Int)

type ('ty, 'env) t =
  (* num of params * num of args *)
  | DifferentArgNum of int * int
  (* target_type * (source_type * source_loc) * ErrorLevel *)
  | ConvErr of ('ty * ('ty * Loc.t) * Function.MatchLevel.t) ArgLocMap.t * 'env
  (* errors * loc *)
  | NoMatch of ('ty, 'env) t list * Loc.t
  | MemberNotFound of 'env * 'env list * Loc.t
  | PackageNotFound of string * (string * string list) list
  | Ambiguous of (Function.MatchLevel.t * 'env list * Loc.t)

  | Msg of string

  | TmpError of string * 'env
