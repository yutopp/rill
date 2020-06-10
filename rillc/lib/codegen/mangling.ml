(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* ref: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling-builtin *)
let mangle_prim_ty ty =
  match ty with
  | Typing.Type.{ ty = Unit; _ } -> "v"
  | Typing.Type.{ ty = Int; _ } -> "i"
  | Typing.Type.{ ty = Bool; _ } -> "b"
  | _ -> failwith "[ICE] not supported yet"

let mangle name ty =
  match ty with
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      let l = String.length name in
      let ps =
        match params with
        | [] -> "v" (* TODO: fix *)
        | _ -> params |> List.map ~f:mangle_prim_ty |> String.concat
      in
      Printf.sprintf "_RRN%d%s%s" l name ps
  | _ -> failwith "[ICE] not supported yet"
