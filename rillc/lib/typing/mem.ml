(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let pointer_size = 8

let rec size_of ~subst ty =
  match Subst.subst_type subst ty with
  | Type.{ ty = Unit; _ } -> 0
  | Type.{ ty = Bool; _ } -> 1
  | Type.{ ty = Int; _ } -> 4
  | Type.{ ty = String; _ } -> pointer_size
  | Type.{ ty = Array { elem; n }; _ } ->
      (* TODO: align *)
      let elem_size = size_of ~subst elem in
      elem_size * n
  | Type.{ ty = Func _; _ } -> pointer_size
  | Type.{ ty = Pointer _; _ } -> pointer_size
  | Type.{ ty = Struct { tag }; _ } ->
      let fields = Subst.get_struct_fields_from_tags subst tag in
      (* TODO: fix *)
      0
  | Type.{ ty = Var _; _ } -> failwith "[ICE] not implemented"
  | _ -> failwith "[ICE] cannot get size of type"