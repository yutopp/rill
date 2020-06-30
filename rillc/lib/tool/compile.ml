(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Workspace = Common.Workspace
module Package = Common.Package
module Triple = Common.Triple
module Os = Common.Os

type t = {
  corelib_srcdir : string option;
  corelib_libdir : string option;
  stdlib_srcdir : string option;
  stdlib_libdir : string option;
  target : Triple.tag_t option;
  out_to : Writer.output_t;
  emit : Emitter.t option;
  pack : bool;
  input_files : string list;
}

(* TODO: fix *)
let host_triple = Triple.Tag_X86_64_unknown_linux_gnu

let default_target_triple = host_triple

let validate opts =
  if List.length opts.input_files < 1 then
    raise (Errors.Invalid_argument "no input file")

let load_builtin_pkg workspace sysroot srcdir pkg_name =
  let pkg_srcdir =
    match srcdir with
    | Some dir -> dir
    | None -> Os.join_path [ sysroot; "lib"; "rill-lib"; "src"; pkg_name ]
  in
  let pkg_id = Workspace.issue_pkg_id ~workspace in
  let pkg = Package.create ~name:pkg_name ~dir:pkg_srcdir ~id:pkg_id in
  Workspace.register_pkg ~workspace pkg;
  let paths =
    Os.grob_dir pkg_srcdir "^.*\\.rill$"
    |> List.map ~f:(fun n -> Stdlib.Filename.concat pkg_srcdir n)
  in
  Package.add_src_paths pkg paths;
  pkg

let entry opts =
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in

  let sysroot = "/usr/local" in

  let target_triple =
    opts.target |> Option.value ~default:default_target_triple
  in

  (* TODO: *)
  let workspace = Workspace.create ~dir:"." ~host_triple ~target_triple in

  (* TODO: fix *)
  let pkg =
    let pkg_id = Workspace.issue_pkg_id ~workspace in
    let pkg = Package.create ~name:"main" ~dir:"." ~id:pkg_id in
    Workspace.register_pkg ~workspace pkg;
    pkg
  in
  Package.add_src_paths pkg opts.input_files;

  (* TODO: fix *)
  let () =
    let dep_pkg =
      load_builtin_pkg workspace sysroot opts.corelib_srcdir "core"
    in
    Package.add_dep_pkg pkg dep_pkg
  in

  (* TODO: fix *)
  let () =
    let dep_pkg = load_builtin_pkg workspace sysroot opts.stdlib_srcdir "std" in
    Package.add_dep_pkg pkg dep_pkg
  in

  let pack = false in
  let compiler =
    let host = Workspace.host ~workspace in
    let target = Workspace.target ~workspace in
    Compiler.create ~workspace ~host ~target
  in

  let%bind () =
    Compiler.compile ~compiler ~format:opts.emit ~printer:Stdio.stderr ~pack
      opts.out_to pkg
  in

  Ok ()
