(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t =
  | ForAll of { implicits : Type.t list; vars : Type.t list; ty : Pred.t }
[@@deriving show, yojson_of]

let of_ty ty = ForAll { implicits = []; vars = []; ty }

let of_trait ~implicits ty = ForAll { implicits; vars = []; ty }

let raw_ty sc =
  let (ForAll { ty; _ }) = sc in
  let (Pred.Pred { ty; _ }) = ty in
  ty

and assume_has_no_generics ty_sc =
  match ty_sc with
  | ForAll { implicits = []; vars = []; ty } -> ty
  | _ -> failwith "NOTE: generics is not allowed here"

let eliminate_type_of ty_sc =
  let (ForAll { ty = pred; implicits; vars }) = ty_sc in
  let (Pred.Pred { ty; conds }) = pred in
  let ty = Type.of_type_ty ty in
  let pred = Pred.Pred { ty; conds } in
  ForAll { ty = pred; implicits; vars }

let rec to_string ty_sc : string =
  match ty_sc with
  | ForAll { implicits = []; vars = []; ty } -> Pred.to_string ty
  | ForAll { implicits; vars; ty } ->
      let implicits_s =
        implicits
        |> List.map ~f:Type.assume_var_id
        |> List.map ~f:Common.Type_var.to_string
        |> String.concat ~sep:","
      in
      let vars_s =
        vars
        |> List.map ~f:Type.assume_var_id
        |> List.map ~f:Common.Type_var.to_string
        |> String.concat ~sep:","
      in
      Printf.sprintf "forall <[%s], i[%s]>.%s" vars_s implicits_s
        (Pred.to_string ty)
