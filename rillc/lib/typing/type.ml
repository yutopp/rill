(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span

type t = {
  ty : ty_t;
  span : (Span.t[@sexp.opaque] (* span that this type is related *));
}

and ty_t = Var of var_t | Unit | Bool | Int | String | Func of t list * t

and var_t = int [@@deriving sexp_of, to_yojson]

let assume_func_ty ty =
  match ty with
  | { ty = Func (params_tys, ret_ty); _ } -> (params_tys, ret_ty)
  | _ -> failwith "[ICE] Not func ty"

(* for debugging. TODO: remove *)
let rec to_string ty : string =
  match ty with
  | { ty = Var v; _ } -> Printf.sprintf "Var %d" v
  | { ty = Unit; _ } -> "Unit"
  | { ty = Bool; _ } -> "Bool"
  | { ty = Int; _ } -> "Int"
  | { ty = String; _ } -> "String"
  | { ty = Func (params, ret_ty); _ } ->
      let params' = List.map params ~f:to_string in
      let ret_ty' = to_string ret_ty in
      let s = String.concat ~sep:" -> " (params' @ [ ret_ty' ]) in
      Printf.sprintf "Func (%s)" s
