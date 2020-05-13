(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type verb_t = Normal | Quiet | Verbose

type t = { debug : bool; verb : verb_t }

module Flags = struct
  open Cmdliner

  let command =
    let docs = Manpage.s_common_options in
    let debug =
      let doc = "Give only debug output." in
      Arg.(value & flag & info [ "debug" ] ~docs ~doc)
    in
    let verb =
      let doc = "Suppress informational output." in
      let quiet = (Quiet, Arg.info [ "q"; "quiet" ] ~docs ~doc) in
      let doc = "Give verbose output." in
      let verbose = (Verbose, Arg.info [ "v"; "verbose" ] ~docs ~doc) in
      Arg.(last & vflag_all [ Normal ] [ quiet; verbose ])
    in

    let gen debug verb = { debug; verb } in
    Term.(const gen $ debug $ verb)
end
[@@warning "-44"]

include Flags
