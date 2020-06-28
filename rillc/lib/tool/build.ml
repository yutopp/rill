(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Diagnostics = Common.Diagnostics
module Workspace = Common.Workspace
module Package = Common.Package
module ProjectFile = Common.Project
module Triple = Common.Triple
module Os = Common.Os

type t = { target : Triple.tag_t option; dir : string }

(* TODO: fix *)
let host_triple = Triple.Tag_X86_64_unknown_linux_gnu

let default_target_triple = host_triple

let validate opts = ()

exception Unexpected_result of string

exception Not_exited_with_code_zero of Unix.process_status

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
    |> List.map ~f:(fun n -> Caml.Filename.concat pkg_srcdir n)
  in
  Package.add_src_paths pkg paths;
  pkg

let rec load_project ~workspace ~pkg_id ~name base_dir =
  let open Result.Let_syntax in
  let mod_file = Os.join_path [ base_dir; "rill.mod.json" ] in
  let ch = Stdlib.open_in mod_file in
  let%bind project =
    Exn.protect
      ~f:(fun () -> ProjectFile.from_channel ch)
      ~finally:(fun () -> Stdlib.close_in ch)
  in

  let deps = project.ProjectFile.deps in
  let%bind dep_pkgs =
    List.fold_result deps ~init:[] ~f:(fun pkgs dep ->
        let ProjectFile.{ dep_name; dep_spec; _ } = dep in

        let ProjectFile.{ dep_spec_path; _ } = dep_spec in
        match dep_spec_path with
        | Some path ->
            let pkg_id = Workspace.issue_pkg_id ~workspace in
            let path = Os.join_path [ base_dir; path ] in
            let%bind pkg =
              load_project ~workspace ~pkg_id ~name:(Some dep_name) path
            in
            Ok (pkg :: pkgs)
        | None -> failwith "[ICE]")
  in

  let pkg =
    let Common.Project.{ name = pkg_name; c_ext; _ } = project in
    let name = Option.value name ~default:pkg_name in
    let dir = base_dir in
    [%loga.debug "pkg : %s -> %s" name dir];

    let pkg = Package.create ~name ~dir ~id:pkg_id in
    Workspace.register_pkg ~workspace pkg;

    let src_paths =
      let pkg_srcdir = Os.join_path [ dir; "src" ] in
      Os.grob_dir pkg_srcdir "^.*\\.rill$"
      |> List.map ~f:(fun p -> Os.join_path [ pkg_srcdir; p ])
    in
    Package.add_src_paths pkg src_paths;
    List.iter src_paths ~f:(fun path -> [%loga.debug " %s -> %s" name path]);

    let () =
      match c_ext with
      | Some c_ext ->
          let ProjectFile.{ c_ext_dir; c_ext_name; _ } = c_ext in
          let ext_src_paths =
            let pkg_srcdir = Os.join_path [ dir; c_ext_dir ] in
            Os.grob_dir pkg_srcdir "^.*\\.c$"
            |> List.map ~f:(fun p -> Os.join_path [ pkg_srcdir; p ])
          in
          let lib_name = Printf.sprintf "lib%s.a" c_ext_name in
          let c_ext = Package.C_ext.create ~lib_name in
          Package.C_ext.add_src_paths c_ext ext_src_paths;

          Package.add_c_ext pkg c_ext
      | None -> ()
    in

    Package.add_dep_pkgs pkg dep_pkgs;

    pkg
  in

  Ok pkg

let build_pkg ~workspace pkg =
  let host = Workspace.host ~workspace in
  let target = Workspace.target ~workspace in
  let compiler = Compiler.create ~workspace ~host ~target in

  let format = None in
  ()

let build_to ~workspace ~out_dir pkg =
  let open Result.Let_syntax in
  let Package.{ c_exts; _ } = pkg in
  let%bind () =
    List.fold_result c_exts ~init:() ~f:(fun _ c_ext ->
        let Package.C_ext.{ lib_name; src_paths_rev; _ } = c_ext in

        let%bind tmp_dir = Os.mktemp_dir "rillc.XXXXXXXX" in
        let%bind obj_paths =
          List.fold_result (src_paths_rev |> List.rev) ~init:[]
            ~f:(fun ps src_path ->
              let obj_name =
                Stdlib.Filename.basename src_path
                |> Stdlib.Filename.chop_extension |> Printf.sprintf "%s.o"
              in
              let obj_path = Os.join_path [ tmp_dir; obj_name ] in

              [%loga.debug "%s -> %s" src_path obj_path];
              let%bind () = Os.cc_obj src_path obj_path in

              Ok (obj_path :: ps))
          |> Result.map ~f:List.rev
        in

        let a_path = Os.join_path [ out_dir; lib_name ] in
        let%bind () = Os.ar obj_paths a_path in

        Package.C_ext.update_artifact c_ext out_dir lib_name;

        Ok ())
  in

  let compiler =
    let host = Workspace.host ~workspace in
    let target = Workspace.target ~workspace in
    Compiler.create ~workspace ~host ~target
  in
  Ok ()

let entry opts =
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in

  let sysroot = "/usr/local" in

  let target_triple =
    opts.target |> Option.value ~default:default_target_triple
  in

  (* TODO: *)
  let workspace = Workspace.create ~dir:opts.dir ~host_triple ~target_triple in

  let pkg_id = Workspace.issue_pkg_id ~workspace in
  let%bind pkg = load_project ~workspace ~pkg_id ~name:None opts.dir in

  let target_dir = Os.join_path [ opts.dir; "_target" ] in
  let () =
    try Unix.mkdir target_dir 0o700
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in

  let%bind () =
    let pkgs = Package.deps_flatten_with_self pkg in
    List.fold_result pkgs ~init:() ~f:(fun _ pkg ->
        let%bind () = build_to ~workspace ~out_dir:target_dir pkg in
        Ok ())
  in

  let compiler =
    let host = Workspace.host ~workspace in
    let target = Workspace.target ~workspace in
    Compiler.create ~workspace ~host ~target
  in

  let obj_path = Os.join_path [ target_dir; "main.o" ] in

  let%bind () =
    let pack = true in
    let out_to = Writer.OutputToFile obj_path in
    Compiler.compile ~compiler ~format:None ~printer:Stdio.stderr ~pack out_to
      pkg
  in

  let (dirs, libnames) =
    let pkgs = Package.deps_flatten_with_self pkg in
    List.map pkgs ~f:(fun pkg ->
        let Package.{ c_exts; _ } = pkg in
        List.filter_map c_exts ~f:(fun c_ext ->
            let Package.C_ext.{ artifact; _ } = c_ext in
            artifact))
    |> List.concat
    |> List.fold_left ~init:([], []) ~f:(fun (dirs, libnames) art ->
           let Package.C_ext.{ dir; filename; _ } = art in
           let libname =
             filename |> Stdlib.Filename.chop_extension
             |> String.chop_prefix_exn ~prefix:"lib"
           in
           (dir :: dirs, libname :: libnames))
  in
  let dirs = Set.of_list (module String) dirs |> Set.to_list in

  let a_path = Os.join_path [ target_dir; "a.out" ] in
  let%bind () = Os.cc_exe dirs libnames [ obj_path ] a_path in

  Ok ()
