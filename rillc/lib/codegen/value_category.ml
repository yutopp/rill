(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type as_treat_t = AsVal | AsPtr of memory_t

and memory_t = MemPrimitive | MemMemory of { size : int } [@@deriving show]

let class_memory_limiy_bytes = 8

let memory_of ty =
  let size = Typing.Mem.size_of ty in
  let mem =
    if size > class_memory_limiy_bytes then MemMemory { size } else MemPrimitive
  in
  mem

let should_treat ty =
  (* TODO: impl *)
  match ty with
  | Typing.Type.{ ty = Array _; _ } | Typing.Type.{ ty = Struct _; _ } ->
      let size = Typing.Mem.size_of ty in
      if size > class_memory_limiy_bytes then AsPtr (memory_of ty) else AsVal
  | Typing.Type.{ ty = Var _; _ } ->
      (* TODO: check Sized trait *)
      failwith "[ICE] cannot determine storage"
  | _ -> AsVal
