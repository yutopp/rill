(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type as_treat_t = AsVal | AsPtr of memory_t

and memory_t = MemPrimitive | MemMemory [@@deriving show]

let memory_of ~subst ty =
  (* TODO: impl *)
  match Typing.Subst.subst_type subst ty with
  | Typing.Type.{ ty = Struct _; _ } -> MemMemory
  | _ -> MemPrimitive

let should_treat ~subst ty =
  (* TODO: impl *)
  match Typing.Subst.subst_type subst ty with
  | Typing.Type.{ ty = Array _; _ } | Typing.Type.{ ty = Func _; _ } ->
      AsPtr (memory_of ~subst ty)
  | Typing.Type.{ ty = Struct _; _ } -> AsPtr (memory_of ~subst ty)
  | Typing.Type.{ ty = Var _; _ } ->
      (* TODO: check Sized trait *)
      failwith "[ICE] cannot determine storage"
  | _ -> AsVal
