(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  tag : Package_tag.t;
  base_dir : string;
  mutable src_paths_rev : string list;
  mutable lib_names_rev : string list;
  mutable deps_rev : Package_tag.t list;
}

let create ~tag ~base_dir () : t =
  { tag; base_dir; src_paths_rev = []; lib_names_rev = []; deps_rev = [] }

let tag s = s.tag

let base_dir s = s.base_dir

let add_src_paths s ~paths =
  (* TODO: check fullpath *)
  s.src_paths_rev <- List.append (paths |> List.rev) s.src_paths_rev

let add_lib_names s ~names =
  s.lib_names_rev <- List.append (names |> List.rev) s.lib_names_rev

let add_dependency s ~tag = s.deps_rev <- tag :: s.deps_rev

let src_paths s = List.rev s.src_paths_rev

let lib_names s = List.rev s.lib_names_rev

let dependencies s = List.rev s.deps_rev
