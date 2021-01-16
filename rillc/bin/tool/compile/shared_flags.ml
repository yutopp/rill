(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Flags = struct
  open Cmdliner

  let sysroot =
    let doc = "" in
    Arg.(value & opt (some dir) None & info [ "sysroot" ] ~doc)

  let target =
    let doc = "" in
    let l = Rillc.Common.Triple.triples_map in
    Arg.(value & opt (some (enum l)) None & info [ "target" ] ~doc)

  let emit =
    let doc = "" in
    let l = Rillc.Compiler.Emitter.emit_map in
    Arg.(value & opt (some (enum l)) None & info [ "emit" ] ~doc)

  let log_level =
    let doc = "" in
    let l =
      [ ("debug", Loga.Severity.Debug); ("error", Loga.Severity.Error) ]
    in
    Arg.(value & opt (enum l) Loga.Severity.Error & info [ "log-level" ] ~doc)

  let output =
    let doc = "" in
    Arg.(value & opt (some string) None & info [ "output"; "o" ] ~doc)

  let files = Arg.(value & (pos_all file) [] & info [] ~docv:"FILES")
end
[@@warning "-44"]

include Flags
