(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* Similer to CxxABI
   ref: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling-structure *)

let rec mangle name ty =
  match ty with
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      Printf.sprintf "_Z%sRill" (nested_name [ name ])
  | _ -> failwith "[ICE] not supported yet"

and nested_name names =
  let names = List.map names ~f:source_name |> String.concat in
  Printf.sprintf "N%sE" names

and source_name id =
  let l = String.length id in
  Printf.sprintf "%d%s" l id

let rec mangle_layer l =
  let Common.Chain.Layer.{ name; generics_vars; _ } = l in
  let generics_s =
    match generics_vars with
    | [] -> ""
    | vars ->
        let sig_tys = vars |> List.map ~f:to_type_sig in
        Printf.sprintf "!(%s)" (String.concat ~sep:"," sig_tys)
  in
  Printf.sprintf "%s%s" name generics_s

and mangle2 nest =
  let names = nest |> List.map ~f:(fun l -> mangle_layer l) in
  Printf.sprintf "_Z%sRill" (nested_name names)

and to_type_sig ty =
  match ty with
  | Typing.Type.{ ty = Unit; _ } -> "unit"
  | Typing.Type.{ ty = Num { bits; signed }; _ } ->
      if signed then Printf.sprintf "i%d" bits else Printf.sprintf "u%d" bits
  | Typing.Type.{ ty = Size { signed }; _ } ->
      if signed then Printf.sprintf "size" else Printf.sprintf "usize"
  | Typing.Type.{ ty = String; _ } -> "string"
  | Typing.Type.{ ty = Array { elem; n }; _ } ->
      Printf.sprintf "[%s; %d]" (to_type_sig elem) n
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      let params' = List.map params ~f:to_type_sig in
      let ret' = to_type_sig ret in
      let s = String.concat ~sep:" -> " params' in
      Printf.sprintf "(%s) -> %s" s ret'
  | Typing.Type.{ ty = Pointer { mut; elem }; _ } ->
      Printf.sprintf "*%s %s" (to_mut_sig mut) (to_type_sig elem)
  | Typing.Type.{ ty = Struct { name }; _ } -> (* TODO: fix *) mangle2 name
  | _ -> failwith "[ICE] cannot encode type"

and to_mut_sig mut : string =
  match mut with
  | Typing.Type.MutImm -> "immutable"
  | Typing.Type.MutMut -> "mutable"
  | _ -> failwith "[ICE] cannot encode mut"
