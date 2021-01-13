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

  let cmd : unit Term.t * Term.info =
    let docs = Manpage.s_common_options in

    let info =
      let doc = "" in
      let man = [ `S Manpage.s_description; `P {||} ] in
      let exits = Term.default_exits in
      Term.info "cc" ~doc ~exits ~man
    in

    let c_linker_flags =
      let doc = "" in
      Arg.(value & (opt_all string) [] & info [ "c-linker-flag" ] ~doc)
    in

    (* -E -S -c *)
    let only_pp =
      let doc = "" in
      Arg.(value & flag & info [ "E" ] ~doc)
    in
    let only_comp =
      let doc = "" in
      Arg.(value & flag & info [ "S" ] ~doc)
    in
    let only_comp_asm =
      let doc = "" in
      Arg.(value & flag & info [ "c" ] ~doc)
    in

    let action sysroot target output only_pp only_comp only_comp_asm
        c_linker_flags input_files =
      let result =
        let opts =
          Rillc.Tool.Cc.
            {
              sysroot;
              target;
              output;
              only_pp;
              only_comp;
              only_comp_asm;
              c_linker_flags;
              input_files;
            }
        in
        Rillc.Tool.Cc.entry opts
        |> Result.map_error ~f:(fun e -> Errors.Flags.Tool_error e)
      in
      match result with Ok _ -> `Ok () | Error e -> Errors.Flags.into_result e
    in
    ( Term.(
        ret
          ( const action $ Shared_flags.sysroot $ Shared_flags.target
          $ Shared_flags.output $ only_pp $ only_comp $ only_comp_asm
          $ c_linker_flags $ Shared_flags.files )),
      info )
end
[@@warning "-44"]
