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
  binding_mut : mutability_t;
  span : (Span.t[@sexp.opaque] (* span that this type is related *));
}

and ty_t =
  | Var of { var : var_t; subst_id : int }
  | Unit
  | Bool
  | Int
  | String
  | Array of { elem : t; n : int }
  | Func of { params : t list; ret : t; linkage : func_linkage_t }
  | Module

and mutability_t = MutImm | MutMut

and var_t = int

and func_linkage_t = LinkageRillc | LinkageC of string | LinkageVar of var_t
[@@deriving show, sexp_of, to_yojson]

let has_no_value ty = match ty with { ty = Unit; _ } -> true | _ -> false

let assume_func_ty ty =
  match ty with
  | { ty = Func { params; ret; _ }; _ } -> (params, ret)
  | _ -> failwith "[ICE] Not func ty"

(* for debugging. TODO: remove *)
let rec to_string ty : string =
  match ty with
  | { ty = Var { var; subst_id }; _ } ->
      Printf.sprintf "Var %d<:%d" var subst_id
  | { ty = Unit; _ } -> "Unit"
  | { ty = Bool; _ } -> "Bool"
  | { ty = Int; _ } -> "Int"
  | { ty = String; _ } -> "String"
  | { ty = Array { elem; n }; _ } ->
      Printf.sprintf "Array[%s; %d]" (to_string elem) n
  | { ty = Func { params; ret; _ }; _ } ->
      let params' = List.map params ~f:to_string in
      let ret' = to_string ret in
      let s = String.concat ~sep:" -> " (params' @ [ ret' ]) in
      Printf.sprintf "Func (%s)" s
  | { ty = Module; _ } -> "Module"
