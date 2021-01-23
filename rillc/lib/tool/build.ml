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
module ProjectFile = Common.Project_file
module Triple = Common.Triple
module Os = Common.Os

type t = { sysroot : string option; target : Triple.tag_t option; dir : string }

let validate opts = ()

let read_project_file base_dir =
  let open Result.Let_syntax in
  let mod_file = Os.join_path [ base_dir; "rill.mod.json" ] in

  let%bind project =
    let ch = Stdlib.open_in mod_file in
    Exn.protect
      ~f:(fun () -> Common.Project_file.from_channel ch)
      ~finally:(fun () -> Stdlib.close_in ch)
  in
  Ok project

let to_pkg ~ws ~project_layout ~deps base_dir =
  (* TODO: support c extention *)
  let Common.Project_file.{ name = pkg_name; _ } = project_layout in
  let name = pkg_name in
  let dir = base_dir in
  [%loga.debug "pkg : %s -> %s" name dir];

  let src_paths =
    let pkg_srcdir = Os.join_path [ dir; "src" ] in
    Os.grob_dir pkg_srcdir "^.*\\.rill$"
    |> List.map ~f:(fun p -> Os.join_path [ pkg_srcdir; p ])
  in

  let pkg_struct =
    let tag = Group.Pkg_tag.create ~name ~version:"latest" in

    let s = Compiler.Structure.create ~tag ~base_dir:"." () in
    Compiler.Structure.add_src_paths s ~paths:src_paths;

    List.iter deps ~f:(fun dep ->
        Compiler.Structure.add_dependency s
          ~tag:(Compiler.Package_handle.tag dep));

    s
  in
  let pkg = Compiler.Workspace.new_pkg ws ~pkg_struct in
  Ok pkg

(*
(* TODO: fix *)
let host_triple = Triple.Tag_X86_64_unknown_linux_gnu

let default_target_triple = host_triple



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

  let%bind project =
    let ch = Stdlib.open_in mod_file in
    Exn.protect
      ~f:(fun () -> Common.Project_file.from_channel ch)
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
    let Common.Project_file.{ name = pkg_name; c_ext; _ } = project in
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
  let compiler = Compiler_.create ~workspace ~host ~target in

  let format = None in
  ()

let build_to ~workspace ~out_dir pkg =
  let target_spec = Common.Target_spec.empty () in

  let open Result.Let_syntax in
  let Package.{ c_exts; _ } = pkg in
  let%bind () =
    List.fold_result c_exts ~init:() ~f:(fun _ c_ext ->
        let Package.C_ext.{ lib_name; src_paths_rev; _ } = c_ext in

        (* TODO: remove tempdir *)
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

        let tmp_a_path = Os.join_path [ tmp_dir; lib_name ] in
        let%bind () =
          Os.ar ~spec:target_spec ~objs:obj_paths ~out:tmp_a_path ()
        in

        let a_path = Os.join_path [ out_dir; lib_name ] in
        let%bind () = Os.cp ~src:tmp_a_path ~dst:a_path in

        Package.C_ext.update_artifact c_ext out_dir lib_name;

        Ok ())
  in

  let compiler =
    let host = Workspace.host ~workspace in
    let target = Workspace.target ~workspace in
    Compiler_.create ~workspace ~host ~target
  in
  Ok ()
 *)

let entry opts =
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in

  let sysroot = Args.sysroot opts.sysroot in

  let host_triple = Args.host_triple () in
  let target_triple = Args.target_triple opts.target in

  let target_sysroot = Args.target_sysroot ~sysroot ~triple:target_triple in

  let%bind target_spec = Args.target_spec ~target_sysroot in

  let ws = Compiler.Workspace.create () in

  (* core *)
  let core_lib = Builtin.new_core_lib ~ws ~sysroot ~srcdir:None in

  (* std *)
  let std_lib = Builtin.new_std_lib ~ws ~sysroot ~srcdir:None ~core_lib in

  let%bind project_layout = read_project_file opts.dir in

  (* TODO: resolve dependencies *)
  let%bind pkg =
    to_pkg ~ws ~project_layout ~deps:[ core_lib; std_lib ] opts.dir
  in

  let target_dir = Os.join_path [ opts.dir; "_target" ] in
  let () =
    try Unix.mkdir target_dir 0o700
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  in

  let printer = Stdio.stderr in

  (* TODO: calc dependency order *)
  let%bind () =
    Compiler.Handler.load_pkg_as_header ~ws ~printer ~pkg_handle:core_lib
  in

  let%bind () =
    Compiler.Handler.load_pkg_as_header ~ws ~printer ~pkg_handle:std_lib
  in

  let (module Target : Triple.PRESET) = target_triple in
  let emitter = Compiler.Emitter.default_emitter_of Target.triple in

  let obj_path = Os.join_path [ target_dir; "main.o" ] in

  let%bind _ =
    let pack = true in
    let out_to = Compiler.Writer.OutputToFile (Some obj_path) in
    Compiler.Handler.compile ~ws ~printer ~pkg_handle:pkg ~pack
      ~triple:target_triple ~emitter ~out_to
  in

  (*
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in

  let sysroot = "/usr/local" in

  let target_triple =
    opts.target |> Option.value ~default:default_target_triple
  in

  let target_spec = Common.Target_spec.empty () in

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
    Compiler_.create ~workspace ~host ~target
  in

  let obj_path = Os.join_path [ target_dir; "main.o" ] in

  let%bind _ =
    let pack = true in
    let out_to = Writer.OutputToFile (Some obj_path) in
    Compiler_.compile ~compiler ~format:None ~printer:Stdio.stderr ~pack out_to
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
  let%bind () =
    Os.cc_exe ~spec:target_spec ~lib_dirs:dirs ~lib_names:libnames
      ~objs:[ obj_path ] ~out:a_path ()
  in
   *)
  Ok ()
