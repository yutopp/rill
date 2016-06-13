(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type compile_options_t = {
  mutable input_files           : string list;
  mutable output_file           : string;

  mutable system_import_dirs    : string list;
  mutable user_import_dirs      : string list;

  mutable options               : string list;
  mutable no_corelib            : bool;
  mutable no_stdlib             : bool;
}

let empty () =
  if Config.use_local_dev_lib then
    {
      input_files = [];
      output_file = "a.out";

      system_import_dirs = ["./stdlib/src"; "./corelib/src"];
      user_import_dirs = [];

      options = [];

      no_corelib = false;
      no_stdlib = false;
    }
  else
    {
      input_files = [];
      output_file = "a.out";

      system_import_dirs = Config.default_includes;
      user_import_dirs = [];

      options = [];

      no_corelib = false;
      no_stdlib = false;
    }


let () =
  (* debug option *)
  Printexc.record_backtrace true;

  (* Compile Option *)
  let co = empty () in

  let usagemsg = "Usage: rillc [filename] <options>\n"; in
  let speclist = [
    ("-o",
     Arg.String (fun s -> co.output_file <- s),
     " specify output file name");
    ("--system-lib",
     Arg.String (fun s -> co.system_import_dirs <- s :: co.system_import_dirs),
     " specify system libs directory");
    ("-I",
     Arg.String (fun s -> co.user_import_dirs <- s :: co.user_import_dirs),
     " specify modules directory");
    ("-L",
     Arg.String (fun s -> co.options <- (Printf.sprintf "-L%s" s) :: co.options),
     " linker option");
    ("-l",
     Arg.String (fun s -> co.options <- (Printf.sprintf "-l%s" s) :: co.options),
     " linker option");
    ("--no-corelib",
     Arg.Unit (fun b -> co.no_corelib <- true),
     " do not link corelib");
    ("--no-stdlib",
     Arg.Unit (fun b -> co.no_stdlib <- true),
     " do not link stdlib");
  ] in
  Arg.parse speclist (fun s -> (co.input_files <- s :: co.input_files)) usagemsg;

  let system_libs_dirs = List.rev co.system_import_dirs in

  let module_search_dirs = List.rev co.user_import_dirs in
  let cur_dir = Sys.getcwd () in
  let module_search_dirs = cur_dir :: module_search_dirs in

  let filepaths =
    let f filename =
      if Filename.is_relative filename then
        Filename.concat cur_dir filename
      else
        filename
    in
    List.map f co.input_files
  in

  (* TODO: fix *)
  assert (List.length co.input_files = 1);
  let filename = List.hd filepaths in

  Printf.printf "===== PHASE = ANALYZE SEMANTICS\n";
  flush_all ();
  let (env, ctx) = Sema.make_default_state system_libs_dirs module_search_dirs in
  let m = Sema.load_module_by_filepath filename ctx in
  let sem_ast = match Sema.analyze_module m ctx with
    | Some node -> node
    | None ->
       Printf.printf "Semantics error\n"; exit 1
  in

  Printf.printf "===== PHASE = CODEGEN\n";
  flush_all ();

  let module Codegen = Codegen_llvm in

  let code_ctx =
    Codegen.make_default_context ~type_sets:ctx.Sema.sc_tsets
                                 ~uni_map:ctx.Sema.sc_unification_ctx
  in
  Codegen.generate sem_ast code_ctx;
  let options =
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
  Codegen.create_executable code_ctx options co.output_file;

  Printf.printf "===== PHASE = FINISHED\n";
  flush_all ();

  exit 0
