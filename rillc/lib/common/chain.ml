(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Layer = struct
  type 'a t = {
    name : string;
    kind : 'a kind_t;
    generics_vars : 'a list;
    has_self : bool;
  }

  and 'a kind_t = Module | Type | Var of 'a [@@deriving show, yojson_of]

  let to_string ~to_s l : string =
    let { name; kind; generics_vars; _ } = l in
    let kind_s = match kind with Module -> "m" | Type -> "t" | Var _ -> "v" in
    let generics_s =
      match generics_vars with
      | [] -> ""
      | vars ->
          Printf.sprintf "!(%s)"
            (vars |> List.map ~f:to_s |> String.concat ~sep:",")
    in
    Printf.sprintf "%s[%s]%s" name kind_s generics_s
end

module Nest = struct
  type 'a t = { paths : 'a Layer.t list; last : 'a Layer.t }
  [@@deriving show, yojson_of]

  let from_list layers =
    let rev_layers = List.rev layers in
    let paths = List.tl_exn rev_layers |> List.rev in
    let last = List.hd_exn rev_layers in
    { paths; last }

  let to_list nest =
    let { paths; last } = nest in
    let layers = last :: List.rev paths in
    layers |> List.rev

  let to_string ~to_s nest =
    let layers = to_list nest in
    layers |> List.map ~f:(Layer.to_string ~to_s) |> String.concat ~sep:"."
end

type 'a t = Local of 'a Layer.t | Global of 'a Nest.t
[@@deriving show, yojson_of]

let to_string ~to_s chain =
  match chain with
  | Local l -> Layer.to_string ~to_s l
  | Global nest -> Nest.to_string ~to_s nest
