(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Compiler

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
     Arg.String (fun s -> co.options <- (Printf.sprintf "-L%s" s) :: co.options),
     "<option> Linker option");
    ("-l",
     Arg.String (fun s -> co.options <- (Printf.sprintf "-l%s" s) :: co.options),
     "<option> Linker option");
    ("--no-corelib",
     Arg.Unit (fun b -> co.no_corelib <- true),
     " Do not link corelib");
    ("--no-stdlib",
     Arg.Unit (fun b -> co.no_stdlib <- true),
     " Do not link stdlib");
    ("-c",
     Arg.Unit (fun b -> co.compile_only <- true),
     " Compile only");
  ] in
  Arg.parse (speclist |> Arg.align)
            (fun s -> co.input_files <- s :: co.input_files)
            usagemsg;

  if co.compile_only && (List.length co.input_files > 1) && Option.is_some co.output_file then
    begin
      Printf.eprintf "cannot specify -o option when multiple files and -c option are given";
      exit 1
    end;

  let filepaths =
    let cur_dir = Sys.getcwd () in
    let f filename =
      if Filename.is_relative filename then
        Filename.concat cur_dir filename
      else
        filename
    in
    List.map f co.input_files
  in

  (* TODO: fix *)
  if List.length co.input_files > 1 then
    begin
      Printf.eprintf "currently, multiple files are not supported: %s\n"
                     (co.input_files |> String.join ", ");
      exit 1
    end;

  assert (List.length co.input_files = 1);
  let filepath = List.hd filepaths in

  let obj_file_name = compile co filepath in

  let build_options =
    let core_lib_opts = if co.no_corelib then []
                        else
                          if Config.use_local_dev_lib then
                            ["-L./corelib/lib"; "-lrillcore-rt"]
                          else
                            [Printf.sprintf "-L%s" Config.default_core_lib_dir;
                             Printf.sprintf "-l%s" Config.default_core_lib_name]
    in
    let std_lib_opts = if co.no_corelib then []
                       else
                         if Config.use_local_dev_lib then
                           ["-L./stdlib/lib"; "-lrillstd-rt"]
                         else
                           [Printf.sprintf "-L%s" Config.default_std_lib_dir;
                            Printf.sprintf "-l%s" Config.default_std_lib_name]
    in
    core_lib_opts @ std_lib_opts @ co.options
  in
  if not co.compile_only then
    let executable_filepath =
      match co.output_file with
      | Some path -> path
      | None ->
         let filepath = List.hd filepaths in
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
