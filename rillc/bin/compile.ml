(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = { common : Common.t; input_files : string list }

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

let entry opts =
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in
  let workspace = Rillc.Workspace.create () in
  let compiler = Rillc.Compiler.create workspace in
  let%bind (compiled, others) =
    Rillc.Compiler.build_from_files compiler ~paths:opts.input_files
    |> Result.map_error ~f:(fun e -> Errors.Failed_to_continue_compiling e)
  in
  let () =
    List.iter ~f:(fun (_, ds) -> log_diagnostics Stdio.stderr ds) compiled
  in
  let () = List.iter ~f:(log_diagnostics_with_last_error Stdio.stderr) others in
  let () = Stdio.Out_channel.flush Stdio.stderr in
  if List.length others = 0 then
    let () =
      let (art, _) = List.hd_exn compiled in
      Rillc.Compiler.dump_artifact art
    in
    Ok ()
  else Error Errors.There_are_warnings_or_errors

module Flags = struct
  open Cmdliner

  let command =
    let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILES") in
    let doc = "create a patch from unrecorded changes" in
    let exits = Term.default_exits in
    let man =
      [
        `S Manpage.s_description;
        `P
          {|
Creates a patch from changes in the working tree. If you specify
        a set of files ...
|};
        `Blocks Help.section;
      ]
    in

    let action common input_files =
      let opts = { common; input_files } in
      match entry opts with
      | Ok v -> `Ok v
      | Error e -> Errors.Flags.into_result e
    in
    ( Term.(ret (const action $ Common.command $ files)),
      Term.info "compile" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )
end
[@@warning "-44"]

include Flags
