(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let pointer_size = 8

let rec size_of ty =
  match ty with
  | Type.{ ty = Unit; _ } -> 0
  | Type.{ ty = Num { bits = 1; _ }; _ } -> (* bool*) 1
  | Type.{ ty = Num { bits; _ }; _ } -> bits / 8
  | Type.{ ty = Size _; _ } ->
      (* TODO: fix, depends on the environment *)
      8
  | Type.{ ty = String; _ } -> pointer_size
  | Type.{ ty = Array { elem; n }; _ } ->
      (* TODO: align *)
      let elem_size = size_of elem in
      elem_size * n
  | Type.{ ty = Func _; _ } -> pointer_size
  | Type.{ ty = Pointer _; _ } -> pointer_size
  | Type.{ ty = Struct { name }; _ } ->
      (* TODO: fix *)
      0
  | Type.{ ty = Var _; _ } -> failwith "[ICE] not implemented"
  | _ -> failwith "[ICE] cannot get size of type"
