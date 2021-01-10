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
      Term.info "rillc_compile" ~version:"%%VERSION%%" ~doc ~exits ~man
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
        target output out_dir emit pack log_level input_files =
      Loga.Logger.set_severity Loga.logger log_level;

      let out_to =
        match (output, out_dir, pack) with
        | (Some _, Some _, _) -> failwith ""
        | (None, None, _) -> failwith ""
        | (Some o, None, _) -> Rillc.Tool.Writer.OutputToFile o
        | (None, Some o, false) -> Rillc.Tool.Writer.OutputToDir o
        | (None, Some _, true) -> failwith ""
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
            out_to;
            emit;
            pack;
            input_files;
          }
      in
      match Rillc.Tool.Compile.entry opts with
      | Ok _ -> `Ok ()
      | Error e -> Errors.Flags.into_result e
    in
    ( Term.(
        ret
          ( const action $ sysroot $ corelib_srcdir $ corelib_libdir
          $ stdlib_srcdir $ stdlib_libdir $ target $ output $ out_dir $ emit
          $ pack $ log_level $ files )),
      info )

  let entry () = Term.(exit @@ eval cmd)
end
[@@warning "-44"]

let () =
  Loga.Logger.set_formatter Loga.logger Caml.Format.err_formatter;
  Flags.entry ()
