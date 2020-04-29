(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let () =
  let opts = Flags.parse () in
  let opts = match Flags.validate opts with
    | Ok opts ->
       opts
    | Error _ ->
       Stdio.eprintf "ERR: args\n"; (* TODO: fix *)
       Caml.exit 1
  in

  let filename = Flags.input_files opts |> List.hd |> Option.value ~default:"DUMMY" in
  Stdio.eprintf "filename: %s\n" filename;

  let workspace = Rillc.create_context () in
  let m = Rillc.build_module workspace filename "out" in

  let dm = Rillc.Module.diagnostics m in
  List.iter ~f:(fun d ->
              Stdio.eprintf "-> %s\n" (Rillc.Diagnostics.to_string d)
            )
            (Rillc.Diagnostics.Multi.to_list dm);
  match Rillc.Module.is_failed m with
  | true ->
     Caml.exit 1
  | false ->
     Caml.exit 0
