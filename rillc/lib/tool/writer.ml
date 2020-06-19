open! Base

type output_t = OutputToFile of string | OutputToDir of string

let with_out_channel ~f ~filepath =
  let open Result.Let_syntax in
  let mode =
    [ Caml.Open_binary; Caml.Open_wronly; Caml.Open_creat; Caml.Open_trunc ]
  in
  let ch = Caml.open_out_gen mode 0o600 filepath in
  let%bind () =
    Exn.protect ~f:(fun () -> f ch) ~finally:(fun () -> Caml.close_out ch)
    |> Result.map_error ~f:(fun s -> Errors.Failed_to_export_artifact "")
  in
  Ok filepath

let out_mod_rel ~f mod_rel =
  let open Result.Let_syntax in
  let (path, ms) = mod_rel in

  let%bind (art, format) =
    let Compiler.ModState.{ phase_result; format; _ } = ms in
    match phase_result with
    | Ok Compiler.ModState.(Artifact art) -> Ok (art, format)
    | _ ->
        (* TODO: fix error messages *)
        Error (Errors.Failed_to_export_artifact "")
  in

  match (art, format) with
  (* Rill-IR *)
  | (Emitter.Artifact.Rill_ir { m }, Some (Emitter.Rill_ir as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch -> Codegen.Rir_gen.write_to ~ch m)
  (* LLVM-IR *)
  | (Emitter.Artifact.Llvm_ir { m }, Some (Emitter.Llvm_ir as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Codegen.Llvm_gen.write_to ~ch ~bitcode:false m)
  (* LLVM-IR bitcode *)
  | (Emitter.Artifact.Llvm_ir { m }, Some (Emitter.Llvm_bc as e)) ->
      let filepath = f ~path e in
      with_out_channel ~filepath ~f:(fun ch ->
          Codegen.Llvm_gen.write_to ~ch ~bitcode:true m)
  | _ ->
      (* TODO: fix error messages *)
      Error (Errors.Failed_to_export_artifact "art and format is unmatched")

let write_pkg_artifacts dict pkg out_to =
  let open Result.Let_syntax in
  let mod_dict = Compiler.PkgDict.get dict ~key:pkg in
  let mod_rels = Compiler.ModDict.to_alist mod_dict in

  let%bind filenames =
    let res =
      match out_to with
      | OutputToFile out_file ->
          let mod_rel =
            match mod_rels with
            | [] -> failwith "[ICE]"
            | [ r ] -> r
            | r :: _ ->
                (* TODO: put warning *)
                r
          in
          let%bind filename =
            out_mod_rel ~f:(fun ~path e -> out_file) mod_rel
          in
          Ok [ filename ]
      | OutputToDir out_dir ->
          List.fold_result mod_rels ~init:[] ~f:(fun files mod_rel ->
              let%bind filename =
                out_mod_rel
                  ~f:(fun ~path e ->
                    let ext = Emitter.ext_of e in
                    let filename =
                      Printf.sprintf "%s.%s" (Caml.Filename.basename path) ext
                    in
                    Caml.Filename.concat out_dir filename)
                  mod_rel
              in
              Ok (filename :: files))
    in
    res
  in
  Ok ()
