(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  name : string;
  dir : string;
  id : id_t;
  mutable deps_rev : t list;
  mutable src_paths_rev : string list;
}

and id_t = int

let create ~name ~dir ~id : t =
  { name; dir; id; deps_rev = []; src_paths_rev = [] }

let base_dir package = package.dir

let add_dep_pkg package dep_pkg =
  package.deps_rev <- dep_pkg :: package.deps_rev

let add_src_paths package paths =
  package.src_paths_rev <- List.append (paths |> List.rev) package.src_paths_rev

let deps package = List.rev package.deps_rev

let src_paths package = List.rev package.src_paths_rev
