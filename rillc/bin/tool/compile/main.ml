(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Flags = struct
  open Cmdliner

  type t = int

  let cmd : unit Term.t * Term.info =
    let docs = Manpage.s_common_options in

    let info =
      let doc = "A compiler tool-chain for Rill language (compile)" in
      let man =
        [ `S Manpage.s_description; `P {|
Compile rill source codes
|} ]
      in
      let exits = Term.default_exits in
      Term.info "rillc" ~version:"%%VERSION%%" ~doc ~exits ~man
    in

    let sysroot =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "sysroot" ] ~docs ~doc)
    in

    let output =
      let doc = "" in
      Arg.(value & opt (some string) None & info [ "output"; "o" ] ~docs ~doc)
    in

    let out_dir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "out_dir" ] ~docs ~doc)
    in

    let corelib_srcdir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "corelib_srcdir" ] ~docs ~doc)
    in

    let corelib_libdir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "corelib_libdir" ] ~docs ~doc)
    in

    let stdlib_srcdir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "stdlib-srcdir" ] ~docs ~doc)
    in

    let stdlib_libdir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "stdlib-libdir" ] ~docs ~doc)
    in

    let target =
      let doc = "" in
      let l = Rillc.Common.Triple.triples_map in
      Arg.(value & opt (some (enum l)) None & info [ "target" ] ~docs ~doc)
    in

    let emit =
      let doc = "" in
      let l = Rillc.Tool.Emitter.emit_map in
      Arg.(value & opt (some (enum l)) None & info [ "emit" ] ~docs ~doc)
    in

    let pack =
      let doc = "" in
      Arg.(value & flag & info [ "pack" ] ~docs ~doc)
    in

    let lib =
      let doc = "" in
      Arg.(value & flag & info [ "lib" ] ~docs ~doc)
    in

    let log_level =
      let doc = "" in
      let l =
        [ ("debug", Loga.Severity.Debug); ("error", Loga.Severity.Error) ]
      in
      Arg.(
        value
        & opt (enum l) Loga.Severity.Error
        & info [ "log-level" ] ~docs ~doc)
    in

    let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILES") in

    let action sysroot corelib_srcdir corelib_libdir stdlib_srcdir stdlib_libdir
        target output out_dir emit pack lib log_level input_files =
      Loga.Logger.set_severity Loga.logger log_level;

      let result =
        let open Result.Let_syntax in
        (* *)
        let%bind () =
          if List.length input_files = 0 then
            Error Errors.Flags.No_input_filenames
          else Ok ()
        in

        (*
         * | output | our_dir |
         * |   x    |    x    | -> FILE: Output an artifact to the generated name.
         * |   o    |    x    | -> FILE: Output an artifact to the given name.
         * |   x    |    o    | -> DIR: Output artifacts under the given directory.
         * |   _    |    _    | -> ERROR
         *)
        let%bind out_to =
          match (output, out_dir) with
          | (None, None) -> Ok (Rillc.Tool.Writer.OutputToFile None)
          | (Some o, None) -> Ok (Rillc.Tool.Writer.OutputToFile (Some o))
          | (None, Some o) -> Ok (Rillc.Tool.Writer.OutputToDir o)
          | _ -> Error Errors.Flags.Cannot_specify_output_and_outdir
        in

        (*
         * | emit | pack | lib |
         * |  o   |  _   |  x  | -> Emit artifact(s) with packed option.
         * |  x   |  !   |  x  | -> Export an executable.
         * |  x   |  !   |  o  | -> Export a library.
         *)
        let%bind export =
          match (emit, pack, lib, out_to) with
          | (Some e, _, false, _) ->
              Ok (Rillc.Tool.Compile.Export.Artifact { emit; pack; out_to })
          | (None, _, false, Rillc.Tool.Writer.OutputToFile out_path) ->
              Ok (Rillc.Tool.Compile.Export.Executable { out_path })
          | (None, _, true, Rillc.Tool.Writer.OutputToFile out_path) ->
              Ok (Rillc.Tool.Compile.Export.Library { out_path })
          | _ -> failwith "[ICE]"
        in

        let opts =
          Rillc.Tool.Compile.
            {
              sysroot;
              corelib_srcdir;
              corelib_libdir;
              stdlib_srcdir;
              stdlib_libdir;
              target;
              export;
              input_files;
            }
        in
        Rillc.Tool.Compile.entry opts
        |> Result.map_error ~f:(fun e -> Errors.Flags.Tool_error e)
      in
      match result with Ok _ -> `Ok () | Error e -> Errors.Flags.into_result e
    in
    ( Term.(
        ret
          ( const action $ sysroot $ corelib_srcdir $ corelib_libdir
          $ stdlib_srcdir $ stdlib_libdir $ target $ output $ out_dir $ emit
          $ pack $ lib $ log_level $ files )),
      info )

  let entry () = Term.(exit @@ eval cmd)
end
[@@warning "-44"]

let () =
  Loga.Logger.set_formatter Loga.logger Caml.Format.err_formatter;
  Flags.entry ()
