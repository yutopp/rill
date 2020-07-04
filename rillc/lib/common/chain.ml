(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Layer = struct
  type 'a t = { name : string; kind : kind_t; generics_vars : 'a list }

  and kind_t = Module | Type | Var [@@deriving show, yojson_of]

  let to_string ~to_s l : string =
    let { name; kind; generics_vars } = l in
    let kind_s = match kind with Module -> "m" | Type -> "t" | Var -> "v" in
    let generics_s =
      match generics_vars with
      | [] -> ""
      | vars ->
          Printf.sprintf "!(%s)"
            (vars |> List.map ~f:to_s |> String.concat ~sep:",")
    in
    Printf.sprintf "%s:%s%s" name kind_s generics_s
end

module Nest = struct
  type 'a t = 'a Layer.t list [@@deriving show, yojson_of]

  let create () = []

  let join_rev nest n = n :: nest

  let to_string ~to_s nest =
    nest |> List.map ~f:(Layer.to_string ~to_s) |> String.concat ~sep:"."
end

type 'a t = Local of 'a Layer.t | Global of 'a Nest.t
[@@deriving show, yojson_of]

let to_string ~to_s chain =
  match chain with
  | Local l -> Layer.to_string ~to_s l
  | Global nest -> Nest.to_string ~to_s nest
