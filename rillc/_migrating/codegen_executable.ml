(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

exception FailedToLinkObjects

module type LINKING =
  sig
    val link : Codegen_option_spec.t list -> string list -> string -> unit
  end


module Linking_cc : LINKING =
  struct
    let list_of_option_spec os =
      let open Codegen_option_spec in
      match os with
      | OsLinkDir s ->
         [Printf.sprintf "-L%s" s]
      | OsLinkLib s ->
         (* static mode -> `lib` -> dynamic mode(default) *)
         ["-Wl,-Bstatic"; Printf.sprintf "-l%s" s; "-Wl,-Bdynamic"]
      | OsRaw s ->
         [s]

    let link options ext_filenames output_filename =
      let string_options =
        options
        |> List.map list_of_option_spec
        |> List.flatten
      in
      let escaped_commands =
        (ext_filenames @ string_options @ ["-pie"; "-o"; output_filename])
        |> List.map Filename.quote
        |> String.concat " "
      in
      let cmd = Printf.sprintf "cc %s" escaped_commands in
      Debug.printf "cmd = %s\n" cmd;
      let sc = Sys.command cmd in
      if sc <> 0 then
        raise FailedToLinkObjects;
      ()
  end



(* output executable *)
let link_objects ext_filenames options output_filename =
  let module Linking = Linking_cc in
  Linking.link options ext_filenames output_filename
