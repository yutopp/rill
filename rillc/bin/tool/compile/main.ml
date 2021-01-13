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

  module Default = struct
    let cmd : unit Term.t * Term.info =
      let doc = "A compiler tool-chain for Rill programming language" in
      let sdocs = Manpage.s_common_options in
      let exits = Term.default_exits in
      let man = [ `S Manpage.s_description; `P {||} ] in
      ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
        Term.info "rillc" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man )
  end

  let entry () =
    Term.(
      exit
      @@ eval_choice Default.cmd
           [
             Subcommand_compile.Flags.cmd;
             Subcommand_build.Flags.cmd;
             Subcommand_cc.Flags.cmd;
             Subcommand_ar.Flags.cmd;
           ])
end
[@@warning "-44"]

let () =
  Loga.Logger.set_formatter Loga.logger Caml.Format.err_formatter;
  Flags.entry ()
