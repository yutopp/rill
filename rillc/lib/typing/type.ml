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
  | Var of { var : Common.Type_var.t; bound : bound_t }
  | Unit
  | Num of { bits : int; signed : bool }
  | Size of { signed : bool }
  | String
  | Array of { elem : t; n : int }
  | Func of { params : t list; ret : t; linkage : func_linkage_t }
  | Pointer of { mut : mutability_t; elem : t }
  | Struct of { name : t Common.Chain.Nest.t }
  (* *)
  | Module
  | Type of t

and bound_t = BoundForall | BoundWeak

and mutability_t = MutImm | MutMut | MutVar of Common.Type_var.t

and func_linkage_t =
  | LinkageRillc
  | LinkageC of string
  | LinkageVar of Common.Type_var.t
[@@deriving show, yojson_of]

let has_no_value ty = match ty with { ty = Unit; _ } -> true | _ -> false

let assume_func_ty ty =
  match ty with
  | { ty = Func { params; ret; _ }; _ } -> (params, ret)
  | _ -> failwith "[ICE] Not func ty"

let assume_var_id ty =
  match ty with
  | { ty = Var { var; _ }; _ } -> var
  | _ -> failwith "[ICE] Not func ty"

let to_type_ty ty = { ty with ty = Type ty }

let of_type_ty ty =
  match ty with { ty = Type ty; _ } -> ty | _ -> failwith "[ICE] not type"

(* for debugging. TODO: remove *)
let rec to_string ty : string =
  match ty with
  | { ty = Var { var; bound }; _ } ->
      let s = match bound with BoundForall -> "'" | BoundWeak -> "W" in
      Printf.sprintf "%s%d" s var
  | { ty = Unit; _ } -> "unit"
  | { ty = Num { bits; signed }; _ } ->
      if signed then Printf.sprintf "i%d" bits else Printf.sprintf "u%d" bits
  | { ty = Size { signed }; _ } ->
      if signed then Printf.sprintf "size" else Printf.sprintf "usize"
  | { ty = String; _ } -> "string"
  | { ty = Array { elem; n }; _ } ->
      Printf.sprintf "Array[%s; %d]" (to_string elem) n
  | { ty = Func { params; ret; _ }; _ } ->
      let params' = List.map params ~f:to_string in
      let ret' = to_string ret in
      let s = String.concat ~sep:" -> " params' in
      Printf.sprintf "fun (%s) -> %s" s ret'
  | { ty = Pointer { mut; elem }; _ } ->
      Printf.sprintf "*%s %s" (to_string_mut mut) (to_string elem)
  | { ty = Struct { name }; _ } ->
      Printf.sprintf "struct %s"
        (Common.Chain.Nest.to_string ~to_s:to_string name)
  | { ty = Module; _ } -> "Module"
  | { ty = Type t; _ } -> "Type"

and to_string_mut mut : string =
  match mut with
  | MutImm -> "immutable"
  | MutMut -> "mutable"
  | MutVar v -> Printf.sprintf "mut_not_determined: %d" v

let to_string_linkage mut : string =
  match mut with
  | LinkageRillc -> "rillc"
  | LinkageC sym -> Printf.sprintf "C: %s" sym
  | LinkageVar v -> Printf.sprintf "linkage_not_determined: %d" v
