(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Flags = struct
  open Cmdliner

  let default_command : unit Term.t * Term.info =
    let doc = "A compiler tool-chain for Rill language" in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    let man = Help.section in
    ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ Common.command)),
      Term.info "rillc" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man )

  let entry () =
    let cmds = [ Compile.command ] in
    Term.(exit @@ eval_choice default_command cmds)
end
[@@warning "-44"]

let () =
  Loga.Logger.set_formatter Loga.default_logger Format.err_formatter;
  Flags.entry ()
