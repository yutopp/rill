(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module C_ext = struct
  type t = {
    lib_name : string;
    mutable src_paths_rev : string list;
    mutable artifact : artifact_t option;
  }

  and artifact_t = { dir : string; filename : string } [@@deriving show]

  let create ~lib_name : t = { lib_name; src_paths_rev = []; artifact = None }

  let add_src_paths ext paths =
    (* TODO: check fullpath *)
    ext.src_paths_rev <- List.append (paths |> List.rev) ext.src_paths_rev

  let update_artifact ext dir filename =
    let art = { dir; filename } in
    ext.artifact <- Some art
end

type t = {
  name : string;
  dir : string;
  id : id_t;
  mutable deps_rev : t list;
  mutable src_paths_rev : string list;
  mutable c_exts : C_ext.t list;
}

and id_t = int [@@deriving show]

let create ~name ~dir ~id : t =
  { name; dir; id; deps_rev = []; src_paths_rev = []; c_exts = [] }

let base_dir package = package.dir

let add_dep_pkg package dep_pkg =
  package.deps_rev <- dep_pkg :: package.deps_rev

let add_dep_pkgs package dep_pkgs =
  package.deps_rev <- List.append (dep_pkgs |> List.rev) package.deps_rev

let add_src_paths package paths =
  (* TODO: check fullpath *)
  package.src_paths_rev <- List.append (paths |> List.rev) package.src_paths_rev

let add_c_ext package c_ext = package.c_exts <- c_ext :: package.c_exts

let deps_flatten package = List.rev package.deps_rev

let deps_flatten_with_self package = List.rev (package :: package.deps_rev)

let src_paths package = List.rev package.src_paths_rev
