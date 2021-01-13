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
      Term.info "ar" ~doc ~exits ~man
    in

    let output =
      Arg.(value & (pos 0 (some string)) None & info [] ~docv:"OUTPUT")
    in
    let files = Arg.(value & (pos_right 0 file) [] & info [] ~docv:"OBJS") in

    let action sysroot target output input_files =
      let result =
        let open Result.Let_syntax in
        (* *)
        let%bind output =
          match output with
          | None -> Error Errors.Flags.No_input_filenames
          | Some v -> Ok v
        in

        let%bind () =
          if List.length input_files = 0 then
            Error Errors.Flags.No_input_filenames
          else Ok ()
        in

        let opts = Rillc.Tool.Ar.{ sysroot; target; output; input_files } in
        Rillc.Tool.Ar.entry opts
        |> Result.map_error ~f:(fun e -> Errors.Flags.Tool_error e)
      in
      match result with Ok _ -> `Ok () | Error e -> Errors.Flags.into_result e
    in
    ( Term.(
        ret
          ( const action $ Shared_flags.sysroot $ Shared_flags.target $ output
          $ files )),
      info )
end
[@@warning "-44"]
