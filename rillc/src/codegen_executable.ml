(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

exception FailedToLinkObjects

(* output executable *)
let link_objects bin_names options out_name =
  let escaped_commands =
    (bin_names @ options @ ["-pie"; "-o"; out_name])
    |> List.map Filename.quote
    |> String.concat " "
  in
  let cmd = Printf.sprintf "g++ %s" escaped_commands in
  Debug.printf "cmd = %s\n" cmd;
  let sc = Sys.command cmd in
  if sc <> 0 then raise FailedToLinkObjects;
  ()
