(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type output_t =
  (* can output a single file *)
  | OutputToFile of string option
  (* can output multiple files *)
  | OutputToDir of string

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

let write_asset asset ~emitter ~f =
  let open Result.Let_syntax in
  let Asset.{ art; path; _ } = asset in

  match (art, emitter) with
  (* Rill-IR *)
  | (Emitter.Artifact.Rill_ir { m }, (Emitter.Rill_ir as e)) ->
      let%bind filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch -> Codegen.Rir_gen.write_to ~ch m)
  (* LLVM-IR *)
  | (Emitter.Artifact.Llvm_ir { m }, (Emitter.Llvm_ir as e)) ->
      let%bind filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.write_to ~ch ~bitcode:false m)
  (* LLVM-IR bitcode *)
  | (Emitter.Artifact.Llvm_ir { m }, (Emitter.Llvm_bc as e)) ->
      let%bind filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.write_to ~ch ~bitcode:true m)
  (* Native: asm *)
  | (Emitter.Artifact.Native { backend; m }, (Emitter.Asm as e)) ->
      let%bind filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.Backend.write_to ~ch ~asm:true backend m)
  (* Native: obj *)
  | (Emitter.Artifact.Native { backend; m }, (Emitter.Obj as e)) ->
      let%bind filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Llvm_gen.Backend.write_to ~ch ~asm:false backend m)
  | _ ->
      (* TODO: fix error messages *)
      Error
        (Errors.Failed_to_export_artifact
           (Printf.sprintf "art and format is unmatched: Art=%s, Format=%s"
              (Emitter.Artifact.tag_string_of art)
              (emitter |> Emitter.ext_of)))

let write_assets ~assets ~emitter ~out_to =
  let gen_pathname_with_ext ~path emitter =
    match path with
    | "" ->
        Error
          (Errors.Failed_to_export_artifact
             "Some of path in artifacts are empty. Please re-run without pack \
              option")
    | _ ->
        let ext = Emitter.ext_of emitter in
        Ok (Printf.sprintf "%s.%s" (Stdlib.Filename.basename path) ext)
  in

  let open Result.Let_syntax in
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
            write_asset ~emitter
              ~f:(fun ~path e ->
                match out_file with
                | Some p -> Ok p
                | None -> gen_pathname_with_ext ~path e)
              asset
          in
          Ok [ filename ]
      (* *)
      | OutputToDir out_dir ->
          List.fold_result assets ~init:[] ~f:(fun files asset ->
              let%bind filename =
                write_asset ~emitter
                  ~f:(fun ~path e ->
                    let%bind filename = gen_pathname_with_ext ~path e in
                    Ok (Stdlib.Filename.concat out_dir filename))
                  asset
              in
              Ok (filename :: files))
    in
    res
  in
  Ok filenames
