(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Pkg_buildspace = Mod_dict

type output_t =
  (* can output a single file *)
  | OutputToFile of string option
  (* can output multiple files *)
  | OutputToDir of string

type asset_t = { art : Emitter.Artifact.t; path : string }

let with_out_channel ~f ~filepath =
  let open Result.Let_syntax in
  let mode =
    [
      Stdlib.Open_binary;
      Stdlib.Open_wronly;
      Stdlib.Open_creat;
      Stdlib.Open_trunc;
    ]
  in
  let ch = Stdlib.open_out_gen mode 0o600 filepath in
  let%bind () =
    Exn.protect ~f:(fun () -> f ch) ~finally:(fun () -> Stdlib.close_out ch)
    |> Result.map_error ~f:(fun _s ->
           Errors.Failed_to_export_artifact "Failed writing data to channel")
  in
  Ok filepath

let write_asset ~format ~f asset =
  let { art; path; _ } = asset in

  match (art, format) with
  (* Rill-IR *)
  | (Emitter.Artifact.Rill_ir { m }, (Emitter.Rill_ir as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch -> Codegen.Rir_gen.write_to ~ch m)
  (* LLVM-IR *)
  | (Emitter.Artifact.Llvm_ir { m }, (Emitter.Llvm_ir as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.write_to ~ch ~bitcode:false m)
  (* LLVM-IR bitcode *)
  | (Emitter.Artifact.Llvm_ir { m }, (Emitter.Llvm_bc as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.write_to ~ch ~bitcode:true m)
  (* Native: asm *)
  | (Emitter.Artifact.Native { backend; m }, (Emitter.Asm as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.Backend.write_to ~ch ~asm:true backend m)
  (* Native: obj *)
  | (Emitter.Artifact.Native { backend; m }, (Emitter.Obj as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.Backend.write_to ~ch ~asm:false backend m)
  | _ ->
      (* TODO: fix error messages *)
      Error
        (Errors.Failed_to_export_artifact
           (Printf.sprintf "art and format is unmatched: Art=%s, Format=%s"
              (Emitter.Artifact.tag_string_of art)
              (format |> Emitter.ext_of)))

let collect_artifacts ~pkg_space =
  let open Result.Let_syntax in
  let mod_rels = Pkg_buildspace.to_alist pkg_space in
  List.fold_result mod_rels ~init:[] ~f:(fun assets (path, ms) ->
      let%bind asset =
        let Mod_state.{ phase_result; _ } = ms in
        match phase_result with
        | Ok Mod_state.(Artifact art) -> Ok { art; path }
        | _ -> Error (Errors.Failed_to_export_artifact "[ICE] not artifacts")
      in
      Ok (asset :: assets))

let pack_if_needed assets =
  let open Result.Let_syntax in
  let%bind llvm_modules =
    List.fold_result assets ~init:[] ~f:(fun mods asset ->
        let { art; _ } = asset in
        match art with
        | Emitter.Artifact.Llvm_ir { m = llvm } -> Ok (llvm :: mods)
        | _ ->
            Error
              (Errors.Failed_to_export_artifact
                 "Cannot pack modules which are not LLVM format"))
  in
  let llvm = Llvm_gen.merge_modules llvm_modules in
  let asset = { path = ""; art = Emitter.Artifact.Llvm_ir { m = llvm } } in
  Ok [ asset ]

let to_natives_if_needed ~triple ~format assets =
  let open Result.Let_syntax in
  let%bind backend =
    Llvm_gen.Backend.create ~triple
    |> Result.map_error ~f:(fun _e ->
           Errors.Failed_to_export_artifact "Could not create LLVM backend")
  in
  let assets =
    List.map assets ~f:(fun asset ->
        match (asset, format) with
        | ({ art = Emitter.Artifact.Llvm_ir { m = llvm }; _ }, Emitter.Asm)
        | ({ art = Emitter.Artifact.Llvm_ir { m = llvm }; _ }, Emitter.Obj) ->
            let art = Emitter.Artifact.Native { backend; m = llvm } in
            { asset with art }
        | _ -> asset)
  in
  Ok assets

let write_pkg_artifacts ~pkg_space ~pack ~triple ~format out_to =
  let open Result.Let_syntax in
  let%bind assets = collect_artifacts ~pkg_space in
  let%bind assets =
    match pack with true -> pack_if_needed assets | false -> Ok assets
  in
  let%bind assets = to_natives_if_needed ~triple ~format assets in

  let%bind () =
    (* Do not allow to write a packed artifact to a dir *)
    match (out_to, pack) with
    | (OutputToDir _, true) ->
        (* TODO: fix error messages *)
        Error
          (Errors.Failed_to_export_artifact
             "Writing a packed asset into a directory is not allowed")
    | _ -> Ok ()
  in

  let gen_pathname_with_ext ~path emitter =
    let ext = Emitter.ext_of emitter in
    Printf.sprintf "%s.%s" (Stdlib.Filename.basename path) ext
  in

  let%bind filenames =
    let res =
      (* *)
      match out_to with
      | OutputToFile out_file ->
          let asset =
            match assets with
            | [] -> failwith "[ICE]"
            | [ asset ] -> asset
            | r :: _ ->
                (* TODO: put warning *)
                r
          in
          let%bind filename =
            write_asset ~format
              ~f:(fun ~path e ->
                match out_file with
                | Some p -> p
                | None -> gen_pathname_with_ext ~path e)
              asset
          in
          Ok [ filename ]
      (* *)
      | OutputToDir out_dir ->
          List.fold_result assets ~init:[] ~f:(fun files asset ->
              let%bind filename =
                write_asset ~format
                  ~f:(fun ~path e ->
                    let filename = gen_pathname_with_ext ~path e in
                    Stdlib.Filename.concat out_dir filename)
                  asset
              in
              Ok (filename :: files))
    in
    res
  in
  Ok filenames
