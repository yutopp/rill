(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Os = Common.Os

let builtin_pkg_info ~sysroot ~srcdir ~lib_names ~name =
  let tag = Compiler.Package_tag.create ~name ~version:"builtin" in

  let pkg_srcdir =
    match srcdir with
    | Some dir -> dir
    | None -> Os.join_path [ sysroot; "lib"; "rill-lib"; "src"; name; "src" ]
  in

  let pkg_struct = Compiler.Structure.create ~tag ~base_dir:pkg_srcdir () in

  let paths =
    Os.grob_dir pkg_srcdir "^.*\\.rill$"
    |> List.map ~f:(fun n -> Stdlib.Filename.concat pkg_srcdir n)
  in
  Compiler.Structure.add_src_paths pkg_struct ~paths;

  Compiler.Structure.add_lib_names pkg_struct ~names:lib_names;

  pkg_struct

let new_core_lib ~ws ~sysroot ~srcdir =
  let pkg_struct =
    builtin_pkg_info ~sysroot ~srcdir ~lib_names:[ "core-c" ] ~name:"core"
  in
  Compiler.Workspace.new_pkg ws ~pkg_struct

let new_std_lib ~ws ~sysroot ~srcdir ~core_lib =
  let pkg_struct =
    let s =
      builtin_pkg_info ~sysroot ~srcdir ~lib_names:[ "std-c" ] ~name:"std"
    in
    Compiler.Structure.add_dependency s
      ~tag:(Compiler.Package_handle.tag core_lib);
    s
  in
  Compiler.Workspace.new_pkg ws ~pkg_struct
