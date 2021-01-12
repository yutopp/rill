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

  let cmd : unit Term.t * Term.info =
    let docs = Manpage.s_common_options in

    let info =
      let doc = "A compiler tool-chain for Rill language (build)" in
      let man = [ `S Manpage.s_description; `P {|
Build rill build codes
|} ] in
      let exits = Term.default_exits in
      Term.info "rillc_build" ~version:"%%VERSION%%" ~doc ~exits ~man
    in

    let target =
      let doc = "" in
      let l =
        Rillc.Common.Triple.triples_map
        |> List.map ~f:(fun (k, v) -> (k, Some v))
      in
      Arg.(value & opt (enum l) None & info [ "target" ] ~docs ~doc)
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

    let dir =
      Arg.(required & pos ~rev:true 0 (some dir) None & info [] ~docv:"FILES")
    in

    let action target log_level dir =
      Loga.Logger.set_severity Loga.logger log_level;

      let opts = Rillc.Tool.Build.{ target; dir } in
      match Rillc.Tool.Build.entry opts with
      | Ok v -> `Ok v
      | Error e -> Errors.Flags.into_result e
    in
    (Term.(ret (const action $ target $ log_level $ dir)), info)

  let entry () = Term.(exit @@ eval cmd)
end
[@@warning "-44"]

let () =
  Loga.Logger.set_formatter Loga.logger Caml.Format.err_formatter;
  Flags.entry ()
