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
