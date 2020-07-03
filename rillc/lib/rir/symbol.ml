(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = Typing.Type.t Common.Chain.Nest.t

let is_generics name =
  name
  |> List.fold_left ~init:false ~f:(fun has l ->
         let Common.Chain.Layer.{ generics_vars; _ } = l in
         let has' = not (List.is_empty generics_vars) in
         has || has')

let has_generics name =
  name
  |> List.fold_left ~init:false ~f:(fun has l ->
         let Common.Chain.Layer.{ generics_vars; _ } = l in
         let has' =
           List.exists generics_vars ~f:(fun v ->
               match v with Typing.Type.{ ty = Var _; _ } -> true | _ -> false)
         in
         has || has')

let rec to_signatured_layer l =
  let Common.Chain.Layer.{ name; kind; generics_vars } = l in
  let kind_s =
    Common.Chain.Layer.(
      match kind with Module -> "m" | Type -> "t" | Var -> "v")
  in
  let generics_s =
    match generics_vars with
    | [] -> ""
    | vars ->
        let sig_tys = vars |> List.map ~f:to_signatured_type in
        Printf.sprintf "!(%s)" (String.concat ~sep:"," sig_tys)
  in
  Printf.sprintf "%s[%s]%s" name kind_s generics_s

and to_signatured_id name =
  name |> List.map ~f:to_signatured_layer |> String.concat ~sep:"."

and to_signatured_type ty =
  match ty with
  | Typing.Type.{ ty = Var { var }; _ } -> Printf.sprintf "'%d" var
  | Typing.Type.{ ty = Unit; _ } -> "unit"
  | Typing.Type.{ ty = Num { bits; signed }; _ } ->
      if signed then Printf.sprintf "i%d" bits else Printf.sprintf "u%d" bits
  | Typing.Type.{ ty = Size { signed }; _ } ->
      if signed then Printf.sprintf "size" else Printf.sprintf "usize"
  | Typing.Type.{ ty = String; _ } -> "string"
  | Typing.Type.{ ty = Array { elem; n }; _ } ->
      Printf.sprintf "[%s; %d]" (to_signatured_type elem) n
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      let params' = List.map params ~f:to_signatured_type in
      let ret' = to_signatured_type ret in
      let s = String.concat ~sep:" -> " params' in
      Printf.sprintf "(%s) -> %s" s ret'
  | Typing.Type.{ ty = Pointer { mut; elem }; _ } ->
      Printf.sprintf "*%s %s" (to_string_mut mut) (to_signatured_type elem)
  | Typing.Type.{ ty = Struct { name }; _ } -> to_signatured_id name
  | Typing.Type.{ ty = Module; _ } -> "Module"
  | Typing.Type.{ ty = Type t; _ } -> "Type"

and to_string_mut mut : string =
  match mut with
  | Typing.Type.MutImm -> "immutable"
  | Typing.Type.MutMut -> "mutable"
  | Typing.Type.MutVar v -> failwith "[ICE]"

let rec to_param_args l =
  let Common.Chain.Layer.{ generics_vars; _ } = l in
  let generics_s =
    match generics_vars with
    | [] -> ""
    | vars ->
        let sig_tys = vars |> List.map ~f:to_signatured_type in
        Printf.sprintf "!(%s)" (String.concat ~sep:"," sig_tys)
  in
  Printf.sprintf "!(%s)" generics_s

and to_param_args_id name =
  let s = name |> List.map ~f:to_param_args |> String.concat ~sep:"." in
  Printf.sprintf "G_%s" s

let to_generic_layer l =
  let Common.Chain.Layer.{ name; kind; _ } = l in
  let kind_s =
    Common.Chain.Layer.(
      match kind with Module -> "m" | Type -> "t" | Var -> "v")
  in
  Printf.sprintf "%s[%s]" name kind_s

let to_generic_id name =
  let s = name |> List.map ~f:to_generic_layer |> String.concat ~sep:"." in
  Printf.sprintf "G_%s" s

let to_id name =
  match is_generics name with
  | true -> to_generic_id name
  | false -> to_signatured_id name
