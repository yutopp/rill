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

type t = {
  corelib_srcdir : string option;
  corelib_libdir : string option;
  stdlib_srcdir : string option;
  stdlib_libdir : string option;
  target : Triple.t option;
  out_to : Writer.output_t;
  emit : Emitter.t option;
  input_files : string list;
}

(* TODO: fix *)
let host_triple = Triple.X86_64_unknown_linux_gnu.triple

let default_target_triple = host_triple

let log_diagnostic_line ch d =
  Diagnostics.Elem.print_for_human ch d;
  Stdio.Out_channel.fprintf ch "\n"

let log_diagnostics ch ds = Diagnostics.iter ~f:(log_diagnostic_line ch) ds

let log_diagnostics_with_last_error ch ((_last_phase, last_error), ds) =
  log_diagnostic_line ch last_error;
  log_diagnostics ch ds

let validate opts =
  if List.length opts.input_files < 1 then
    raise (Errors.Invalid_argument "no input file")

let read_dir_names dir =
  let handle = Unix.opendir dir in
  Exn.protectx
    ~f:(fun handle ->
      let rec f (acc : string list) =
        try
          let name = Unix.readdir handle in
          f (name :: acc)
        with End_of_file -> acc
      in
      f [])
    handle ~finally:Unix.closedir

(* TODO: check is_file/is_dir *)
let grob_dir dir pattern =
  let reg = Str.regexp pattern in
  let names = read_dir_names dir in
  List.filter names ~f:(fun name -> Str.string_match reg name 0)

(* TODO: fix *)
let join_path paths =
  match paths with
  | [] -> ""
  | x :: xs -> List.fold_left xs ~init:x ~f:Caml.Filename.concat

let load_builtin_pkg workspace sysroot srcdir pkg_name =
  let pkg_srcdir =
    match srcdir with
    | Some dir -> dir
    | None -> join_path [ sysroot; "lib"; "rill-lib"; "src"; pkg_name ]
  in
  let pkg_id = Workspace.issue_pkg_id ~workspace in
  let pkg = Package.create ~name:pkg_name ~dir:pkg_srcdir ~id:pkg_id in
  Workspace.register_pkg ~workspace pkg;
  let paths =
    grob_dir pkg_srcdir "^.*\\.rill$"
    |> List.map ~f:(fun n -> Caml.Filename.concat pkg_srcdir n)
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
  let workspace = Workspace.create () in

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

  let compiler =
    Compiler.create ~workspace ~host:host_triple ~target:target_triple
  in

  let%bind () =
    let dict = Compiler.build_pkg compiler pkg ~format:opts.emit in
    let pkg_rels = Compiler.PkgDict.to_alist dict in
    List.iter pkg_rels ~f:(fun (pkg, mod_dict) ->
        let mod_rels = Compiler.ModDict.to_alist mod_dict in
        List.iter mod_rels ~f:(fun (path, ms) ->
            let m = ms.Compiler.ModState.m in
            let ds = m.Compiler.Mod.ds in

            let () =
              let res = ms.Compiler.ModState.phase_result in
              match res with
              | Ok _ -> log_diagnostics Stdio.stderr ds
              | Error failed ->
                  log_diagnostics_with_last_error Stdio.stderr (failed, ds)
            in
            ()));
    let () = Stdio.Out_channel.flush Stdio.stderr in
    if compiler.Compiler.has_fatal then
      (* Error Errors.There_are_warnings_or_errors *)
      Caml.exit 1;

    let%bind _ = Writer.write_pkg_artifacts dict pkg opts.out_to in
    Ok ()
  in
  Ok ()
