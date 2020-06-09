(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Flags = struct
  open Cmdliner

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

    let out_dir =
      let doc = "" in
      Arg.(required & opt (some dir) None & info [ "out_dir" ] ~docs ~doc)
    in

    let corelib_srcdir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "corelib_srcdir" ] ~docs ~doc)
    in

    let corelib_libdir =
      let doc = "" in
      Arg.(value & opt (some dir) None & info [ "corelib_libdir" ] ~docs ~doc)
    in

    let emit =
      let doc = "" in
      let l =
        [
          ("rill-ir", Rillc.Tool.Compile.EmitRillIr);
          ("llvm-ir", Rillc.Tool.Compile.EmitLLVMIr);
          ("llvm-ir-bc", Rillc.Tool.Compile.EmitLLVMIrBc);
        ]
      in
      Arg.(
        value
        & opt (enum l) Rillc.Tool.Compile.EmitLLVMIrBc
        & info [ "emit" ] ~docs ~doc)
    in

    let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILES") in

    let action corelib_srcdir corelib_libdir out_dir emit input_files =
      let opts =
        Rillc.Tool.Compile.
          { corelib_srcdir; corelib_libdir; out_dir; emit; input_files }
      in
      match Rillc.Tool.Compile.entry opts with
      | Ok v -> `Ok v
      | Error e -> Errors.Flags.into_result e
    in
    ( Term.(
        ret
          ( const action $ corelib_srcdir $ corelib_libdir $ out_dir $ emit
          $ files )),
      info )

  let entry () = Term.(exit @@ eval cmd)
end
[@@warning "-44"]

let () =
  Loga.Logger.set_formatter Loga.logger Format.err_formatter;
  Flags.entry ()
