(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = Pred of { conds : cond_t list; ty : Type.t }

and cond_t = { cond_trait : Type.t; cond_var : Type.t }
[@@deriving show, yojson_of]

let of_type ty = Pred { conds = []; ty }

let to_type pty =
  let (Pred { ty; _ }) = pty in
  ty

let with_span ~span pty =
  let (Pred { conds; ty }) = pty in
  let ty = Type.{ ty with span } in
  Pred { conds; ty }

let rec to_string pty : string =
  let (Pred { conds; ty }) = pty in
  Printf.sprintf "(conds: [%s]).%s"
    (conds |> List.map ~f:to_string_cond |> String.concat ~sep:",")
    (Type.to_string ty)

and to_string_cond cond : string =
  let { cond_trait = trait; cond_var = var } = cond in
  Printf.sprintf "%s<%s>" (Type.to_string trait) (Type.to_string var)
