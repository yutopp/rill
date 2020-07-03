(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = ForAll of Type.t list * Type.t [@@deriving show, yojson_of]

let of_ty ty = ForAll ([], ty)

let raw_ty sc =
  let (ForAll (_, ty)) = sc in
  ty

and assume_has_no_generics ty_sc =
  let (ForAll (ty_vars, ty)) = ty_sc in
  match ty_vars with
  | [] -> ty
  | _ -> failwith "NOTE: generics is not allowed here"

let rec to_string ty_sc : string =
  let (ForAll (ty_vars, ty)) = ty_sc in
  match ty_vars with
  | [] -> Type.to_string ty
  | vars ->
      let s =
        vars
        |> List.map ~f:Type.assume_var_id
        |> List.map ~f:Int.to_string |> String.concat ~sep:","
      in
      Printf.sprintf "forall (%s).%s" s (Type.to_string ty)
