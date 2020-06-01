(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  common : Common.t;
  include_dir : string list;
  out_dir : string option;
  input_files : string list;
}

let log_diagnostic_line ch d =
  Rillc.Diagnostics.Elem.print_for_human ch d;
  Stdio.Out_channel.fprintf ch "\n"

let log_diagnostics ch ds =
  Rillc.Diagnostics.iter ~f:(log_diagnostic_line ch) ds

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

let entry opts =
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in

  (* TODO: *)
  let workspace = Rillc.Workspace.create () in

  (* TODO: fix *)
  let core_pkg =
    if List.length opts.include_dir <> 1 then failwith "";
    let core_dir = List.hd_exn opts.include_dir in
    let pkg_id = Rillc.Workspace.issue_pkg_id ~workspace in
    let pkg = Rillc.Package.create ~name:"core" ~dir:core_dir ~id:pkg_id in
    Rillc.Workspace.register_pkg ~workspace pkg;
    let paths = grob_dir core_dir "^.*\\.rill$" in
    Rillc.Package.add_src_paths pkg paths;
    pkg
  in

  (* TODO: fix *)
  let pkg =
    let pkg_id = Rillc.Workspace.issue_pkg_id ~workspace in
    Rillc.Package.create ~name:"a" ~dir:"" ~id:pkg_id
  in
  Rillc.Package.add_dep_pkg pkg core_pkg;
  Rillc.Package.add_src_paths pkg opts.input_files;

  let compiler = Rillc.Compiler.create workspace in

  let () =
    let dict = Rillc.Compiler.build_pkg compiler pkg in
    let pkg_rels = Rillc.Compiler.PkgDict.to_alist dict in
    List.iter pkg_rels ~f:(fun (pkg, mod_dict) ->
        let mod_rels = Rillc.Compiler.ModDict.to_alist mod_dict in
        List.iter mod_rels ~f:(fun (path, ms) ->
            let m = ms.Rillc.Compiler.ModState.m in
            let ds = m.Rillc.Compiler.Mod.ds in

            let () =
              let res = ms.Rillc.Compiler.ModState.phase_result in
              match res with
              | Ok _ -> log_diagnostics Stdio.stderr ds
              | Error failed ->
                  log_diagnostics_with_last_error Stdio.stderr (failed, ds)
            in
            ()));
    if compiler.Rillc.Compiler.has_fatal then
      (*Error Errors.There_are_warnings_or_errors*)
      Caml.exit 1;

    let mod_dict = Rillc.Compiler.PkgDict.get dict ~key:pkg in
    let mod_rels = Rillc.Compiler.ModDict.to_alist mod_dict in
    List.iter mod_rels ~f:(fun (path, ms) ->
        match ms.Rillc.Compiler.ModState.phase_result with
        | Ok (Rillc.Compiler.ModState.ArtifactRir rir) ->
            Rillc.Compiler.dump_rir rir
        | Ok (Rillc.Compiler.ModState.ArtifactLlvm llvm) ->
            Rillc.Compiler.dump_llvm llvm
        | _ -> ())
  in
  let () = Stdio.Out_channel.flush Stdio.stderr in
  Ok ()

module Flags = struct
  open Cmdliner

  let command =
    let docs = Manpage.s_common_options in
    let subcommand =
      let doc = "compile rill source codes" in
      let exits = Term.default_exits in
      let man =
        [
          `S Manpage.s_description;
          `P {|
Compile rill source codes
|};
          `Blocks Help.section;
        ]
      in
      Term.info "compile" ~doc ~sdocs:docs ~exits ~man
    in

    let include_dir =
      let doc = "" in
      Arg.(value & opt_all dir [] & info [ "I" ] ~docs ~doc)
    in

    let out_dir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "d"; "out_dir" ] ~docs ~doc)
    in

    let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILES") in

    let action common include_dir out_dir input_files =
      let opts = { common; include_dir; out_dir; input_files } in
      match entry opts with
      | Ok v -> `Ok v
      | Error e -> Errors.Flags.into_result e
    in
    ( Term.(ret (const action $ Common.command $ include_dir $ out_dir $ files)),
      subcommand )
end
[@@warning "-44"]

include Flags
