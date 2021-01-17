(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Name = struct
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

type 'a t = {
  pkg_tag : Group.Pkg_tag.t;
  (* if paths is nil, that is local path *)
  paths : 'a Name.t list;
  last : 'a Name.t;
}
[@@deriving show, yojson_of]

let create ~pkg_tag names =
  let rev_names = List.rev names in
  let paths = List.tl_exn rev_names |> List.rev in
  let last = List.hd_exn rev_names in
  { pkg_tag; paths; last }

let to_list path =
  let { paths; last; _ } = path in
  let names = last :: List.rev paths in
  names |> List.rev

let to_string ~to_s path =
  let names = to_list path in
  names |> List.map ~f:(Name.to_string ~to_s) |> String.concat ~sep:"."
