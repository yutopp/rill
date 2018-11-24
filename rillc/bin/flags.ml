(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  (* input filepaths for compilation *)
  mutable input_files                       : string list;
  (* output filepath *)
  mutable output_file                       : string option;
  (* if true, do only compilation *)
  mutable compile_only                      : bool;
}

let input_files opts = opts.input_files

let default () : t=
  {
    input_files = [];
    output_file = None;
    compile_only = false;
  }

let usagemsg = "Usage: rillc <options> [filenames]\n"

let specs co = [
  ("-o",
   Arg.String (fun s -> co.output_file <- Some s),
   "<path> specify an output file name");
  ("-c",
   Arg.Unit (fun () -> co.compile_only <- true),
   " Compile only");
]

let parse () : t =
  let opts = default () in
  let () =
    Arg.parse (specs opts |> Arg.align ~limit:max_int)
              (fun s -> opts.input_files <- s :: opts.input_files)
              usagemsg
  in
  opts

let validate opts : (t, unit) result =
  Ok opts
