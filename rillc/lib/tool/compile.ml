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
  out_to : output_t;
  emit : emit_t;
  input_files : string list;
}

and emit_t = EmitRillIr | EmitLLVMIr | EmitLLVMIrBc

and output_t = OutputFile of string | OutputDir of string

let host_triple = "x86_64_unknown-linux-gnu"

let target_triple = host_triple

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
let join_path paths = String.concat ~sep:"/" paths

let tmp_out_channel ~f ~filepath =
  let open Result.Let_syntax in
  let mode = [ Caml.Open_binary; Caml.Open_wronly; Caml.Open_creat ] in
  let ch = Caml.open_out_gen mode 0o600 filepath in
  let%bind () =
    Exn.protect ~f:(fun () -> f ch) ~finally:(fun () -> Caml.close_out ch)
    |> Result.map_error ~f:(fun s -> Errors.Failed_to_export_artifact "")
  in
  Ok filepath

let out_mod_rel ~emit ~f mod_rel =
  let (path, ms) = mod_rel in
  let filepath = f path in
  match ms.Compiler.ModState.phase_result with
  (* Rill-IR *)
  | Ok (Compiler.ModState.ArtifactRir rir) when Poly.equal emit EmitRillIr ->
      tmp_out_channel ~filepath ~f:(fun ch -> Codegen.Rir_gen.write_to ~ch rir)
  (* LLVM-IR *)
  | Ok (Compiler.ModState.ArtifactLlvm llvm) when Poly.equal emit EmitLLVMIr ->
      tmp_out_channel ~filepath ~f:(fun ch ->
          Codegen.Llvm_gen.write_to ~ch ~bitcode:false llvm)
  (* LLVM-IR bitcode *)
  | Ok (Compiler.ModState.ArtifactLlvm llvm) when Poly.equal emit EmitLLVMIrBc
    ->
      tmp_out_channel ~filepath ~f:(fun ch ->
          Codegen.Llvm_gen.write_to ~ch ~bitcode:true llvm)
  | _ -> Error (Errors.Failed_to_export_artifact "")

let load_builtin_pkg workspace sysroot srcdir pkg_name =
  let pkg_srcdir =
    match srcdir with
    | Some dir -> dir
    | None -> join_path [ sysroot; "/lib/rill-lib/src"; pkg_name ]
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

  let compiler = Compiler.create workspace in

  let%bind () =
    let code_gen_phase =
      match opts.emit with
      | EmitRillIr -> Compiler.CodegenPhaseRir
      | EmitLLVMIr | EmitLLVMIrBc -> Compiler.CodegenPhaseLLVM
    in
    let dict = Compiler.build_pkg compiler pkg ~code_gen_phase in
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

    let mod_dict = Compiler.PkgDict.get dict ~key:pkg in
    let mod_rels = Compiler.ModDict.to_alist mod_dict in
    let%bind filenames =
      let emit = opts.emit in
      let res =
        match opts.out_to with
        | OutputFile out_file ->
            let mod_rel =
              match mod_rels with
              | [] -> failwith "[ICE]"
              | [ r ] -> r
              | r :: _ ->
                  (* TODO: put warning *)
                  r
            in
            let%bind filename =
              out_mod_rel ~emit ~f:(fun _ -> out_file) mod_rel
            in
            Ok [ filename ]
        | OutputDir out_dir ->
            List.fold_result mod_rels ~init:[] ~f:(fun files mod_rel ->
                let%bind filename =
                  out_mod_rel ~emit
                    ~f:(fun path ->
                      let ext =
                        match emit with
                        | EmitRillIr -> "rir"
                        | EmitLLVMIr -> "ll"
                        | EmitLLVMIrBc -> "bc"
                      in
                      let filename =
                        Printf.sprintf "%s.%s" (Caml.Filename.basename path) ext
                      in
                      join_path [ out_dir; filename ])
                    mod_rel
                in
                Ok (filename :: files))
      in
      res
    in
    Ok ()
  in
  Ok ()
