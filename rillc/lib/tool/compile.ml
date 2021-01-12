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
module Target_spec = Common.Target_spec
module Os = Common.Os

module Export = struct
  type t =
    | Artifact of {
        emit : Emitter.t option;
        pack : bool;
        out_to : Writer.output_t;
      }
    | Executable of { out_path : string option }
    | Library of { out_path : string option }
end

type t = {
  sysroot : string option;
  corelib_srcdir : string option;
  corelib_libdir : string option;
  stdlib_srcdir : string option;
  stdlib_libdir : string option;
  target : Triple.tag_t option;
  export : Export.t;
  input_files : string list;
}

let validate opts =
  if List.length opts.input_files < 1 then
    raise (Errors.Invalid_argument "no input file")

let load_builtin_pkg workspace sysroot srcdir pkg_name =
  let pkg_srcdir =
    match srcdir with
    | Some dir -> dir
    | None ->
        Os.join_path [ sysroot; "lib"; "rill-lib"; "src"; pkg_name; "src" ]
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

  let sysroot = Args.sysroot opts.sysroot in

  let host_triple = Args.host_triple () in
  let target_triple = Args.target_triple opts.target in

  let target_sysroot = Args.target_sysroot ~sysroot ~triple:target_triple in

  let%bind target_spec = Args.target_spec ~target_sysroot in

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
  let core_lib =
    let dep_pkg =
      load_builtin_pkg workspace sysroot opts.corelib_srcdir "core"
    in
    Package.add_dep_pkg pkg dep_pkg;
    "core-c"
  in

  (* TODO: fix *)
  let std_lib =
    let dep_pkg = load_builtin_pkg workspace sysroot opts.stdlib_srcdir "std" in
    Package.add_dep_pkg pkg dep_pkg;
    "std-c"
  in

  let compiler =
    let host = Workspace.host ~workspace in
    let target = Workspace.target ~workspace in
    Compiler.create ~workspace ~host ~target
  in

  (* adhoc-impl *)
  let lib_dirs = [ Os.join_path [ target_sysroot; "lib" ] ] in
  let lib_names = [ core_lib; std_lib ] in

  let printer = Stdio.stderr in
  let%bind () =
    match opts.export with
    | Export.Artifact { emit = format; pack; out_to } ->
        let%bind _ =
          Compiler.compile ~compiler ~format ~printer ~pack out_to pkg
        in
        Ok ()
    | Export.Executable { out_path } ->
        let%bind tmp_dir = Os.mktemp_dir "rillc.XXXXXXXX" in
        let format = None in
        let pack = false in
        let out_to = Writer.OutputToDir tmp_dir in
        let%bind filenames =
          Compiler.compile ~compiler ~format ~printer ~pack out_to pkg
        in
        [%loga.debug "outputs = %s" (String.concat ~sep:"; " filenames)];
        let out = out_path |> Option.value ~default:"a.out" in
        let%bind () =
          Os.cc_exe ~spec:target_spec ~lib_dirs ~lib_names ~objs:filenames ~out
            ()
        in
        Ok ()
    | _ -> failwith ""
  in

  Ok ()
