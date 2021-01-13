(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  sysroot : string option;
  target : Common.Triple.tag_t option;
  log_level : Loga.Severity.t;
  dir : string;
}

let entry opts =
  let { sysroot; target; log_level; dir } = opts in
  Loga.Logger.set_severity Loga.logger log_level;

  let opts = Rillc.Tool.Build.{ target; dir } in
  Rillc.Tool.Build.entry opts
  |> Result.map_error ~f:(fun e -> Errors.Flags.Tool_error e)

module Flags = struct
  open Cmdliner

  let cmd : unit Term.t * Term.info =
    let info =
      let doc = "build" in
      let man = [ `S Manpage.s_description; `P {|
Build rill project.
|} ] in
      let exits = Term.default_exits in
      Term.info "build" ~doc ~exits ~man
    in

    let dir =
      Arg.(required & pos ~rev:true 0 (some dir) None & info [] ~docv:"FILES")
    in

    let action sysroot target log_level dir =
      let result = entry { sysroot; target; log_level; dir } in
      match result with Ok _ -> `Ok () | Error e -> Errors.Flags.into_result e
    in
    ( Term.(
        ret
          ( const action $ Shared_flags.sysroot $ Shared_flags.target
          $ Shared_flags.log_level $ dir )),
      info )
end
[@@warning "-44"]
