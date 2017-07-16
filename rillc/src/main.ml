(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Compiler

let () =
  Debug.record_backtrace ();

  (* exit if failed to parse options *)
  let co = Compiler.Arg.parse () in
  match Compiler.build co with
  | Ok _ ->
     exit 0
  | Bad errors ->
     List.iter (fun err -> Printf.eprintf "ERROR: %s\n" err) errors;
     exit 1
