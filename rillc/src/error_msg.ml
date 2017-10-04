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
  | ConvErr of ('ty * ('ty * Loc.t) * 'env)
  (* target_type * (source_type * source_loc) * ErrorLevel *)
  | ArgConvErr of ('ty * ('ty * Loc.t) * Function.MatchLevel.t) ArgLocMap.t * 'env
  (* errors * loc *)
  | NoOverloadSet of ('ty, 'env) t list * Loc.t
  | NoMatch of ('ty, 'env) t list * Loc.t
  | MemberNotFound of 'env * 'env list * Loc.t
  | ModuleNotFound of string * (string * string list) list * Loc.t
  (* expect * actual *)
  | ModuleNameDifferent of (string list * string) * (string list * string)

  | Ambiguous of (Function.MatchLevel.t * 'env list * Loc.t)

  | MultiSymbol of 'env * Loc.t

  | DiffExecLevel of {loc: Loc.t; expect: Meta_level.t; actual: Meta_level.t}

  | Msg of string

  | TmpError of string * 'env
