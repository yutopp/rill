(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

exception FailedToLinkObjects

let string_of_option_spec os =
  let open Codegen_option_spec in
  match os with
  | OsLinkDir s ->
     Printf.sprintf "-L%s" s
  | OsLinkLib s ->
     (* static mode[-dn] -> `lib` -> dynamic mode[-dy](default) *)
     Printf.sprintf "-Wl,-dn,-l%s,-dy" s
  | OsRaw s -> s

(* output executable *)
let link_objects bin_names options out_name =
  let string_options = options |> List.map string_of_option_spec in
  let escaped_commands =
    (bin_names @ string_options @ ["-pie"; "-o"; out_name])
    |> List.map Filename.quote
    |> String.concat " "
  in
  let cmd = Printf.sprintf "g++ %s" escaped_commands in
  Debug.printf "cmd = %s\n" cmd;
  let sc = Sys.command cmd in
  if sc <> 0 then raise FailedToLinkObjects;
  ()
