(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Compiler
module COS = Codegen_option_spec

let empty () =
  if Config.use_local_dev_lib then
    {
      input_files = [];
      output_file = None;

      system_import_dirs = ["./stdlib/src"; "./corelib/src"];
      user_import_dirs = [];

      options = [];

      no_corelib = false;
      no_stdlib = false;

      compile_only = false;
    }
  else
    {
      input_files = [];
      output_file = None;

      system_import_dirs = Config.default_includes;
      user_import_dirs = [];

      options = [];

      no_corelib = false;
      no_stdlib = false;

      compile_only = false;
    }

let show_version_and_exit () =
  Printf.printf "rillc %s\n%!" Config.version;
  exit 0

let () =
  Debug.record_backtrace ();

  (* Compile Option *)
  let co = empty () in

  let usagemsg = "Usage: rillc <options> [filename]\n"; in
  let speclist = [
    ("-o",
     Arg.String (fun s -> co.output_file <- Some s),
     "<path> specify output file name");
    ("--system-lib",
     Arg.String (fun s -> co.system_import_dirs <- s :: co.system_import_dirs),
     "<dir> Specify system libs directory");
    ("-I",
     Arg.String (fun s -> co.user_import_dirs <- s :: co.user_import_dirs),
     "<dir> Specify modules directory");
    ("-L",
     Arg.String (fun s -> co.options <- (COS.OsLinkDir s) :: co.options),
     "<option> Linker option");
    ("-l",
     Arg.String (fun s -> co.options <- (COS.OsLinkLib s) :: co.options),
     "<option> Linker option");
    ("--no-corelib",
     Arg.Unit (fun () -> co.no_corelib <- true),
     " Do not link corelib");
    ("--no-stdlib",
     Arg.Unit (fun () -> co.no_stdlib <- true),
     " Do not link stdlib");
    ("-c",
     Arg.Unit (fun () -> co.compile_only <- true),
     " Compile only");
    ("--version",
     Arg.Unit show_version_and_exit,
     " Show version");
  ] in
  Arg.parse (speclist |> Arg.align)
            (fun s -> co.input_files <- s :: co.input_files)
            usagemsg;

  if co.compile_only && (List.length co.input_files > 1) && Option.is_some co.output_file then
    begin
      Printf.eprintf "cannot specify -o option when multiple files and -c option are given";
      exit 1
    end;

  (* TODO: fix *)
  if List.length co.input_files > 1 then
    begin
      Printf.eprintf "currently, multiple files are not supported: %s\n"
                     (co.input_files |> String.join ", ");
      exit 1
    end;
  assert (List.length co.input_files = 1);

  let filepath = List.hd co.input_files in (* TODO: support multiple files *)

  let build_options = make_build_options co in

  let obj_file_name = compile co build_options filepath in

  if not co.compile_only then
    let executable_filepath =
      match co.output_file with
      | Some path -> path
      | None ->
         try
           filepath
           |> Filename.basename
           |> Filename.chop_extension
         with
         | Invalid_argument _ -> filepath
    in
    Codegen_executable.link_objects [obj_file_name] build_options executable_filepath;

  Debug.printf "===== PHASE = FINISHED\n";

  exit 0
