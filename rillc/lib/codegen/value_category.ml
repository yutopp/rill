(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type as_treat_t = AsVal | AsPtr [@@deriving show]

let should_treat ty =
  (* TODO: impl *)
  match ty with
  | Typing.Type.{ ty = Array _; _ }
  | Typing.Type.{ ty = Func _; _ }
  | Typing.Type.{ ty = Struct _; _ } ->
      AsPtr
  | _ -> AsVal
