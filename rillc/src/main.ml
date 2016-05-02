(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type compile_options_t = {
  mutable input_files       : string list;
  mutable output_file       : string;

  mutable system_import_dir_core    : string;
  mutable system_import_dir_std     : string;
  mutable system_import_dirs        : string list;

  mutable user_import_dirs          : string list;

  mutable system_default_link_option    : string
}

let empty () =
  {
    input_files = [];
    output_file = "a.out";

    system_import_dir_core = "./corelib/src";
    system_import_dir_std  = "./stdlib/src";
    system_import_dirs = [];

    user_import_dirs = [];

    system_default_link_option = "-L./stdlib/lib -lrillstd-rt -L./corelib/lib -lrillcore-memory"
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
    ("--system-lib-core",
     Arg.String (fun s -> co.system_import_dir_core <- s),
     " specify core system lib directory");
    ("--system-lib-std",
     Arg.String (fun s -> co.system_import_dir_std <- s),
     " specify std system lib directory");
    ("--system-lib",
     Arg.String (fun s -> co.system_import_dirs <- s :: co.system_import_dirs),
     " specify system libs directory");
    ("-I",
     Arg.String (fun s -> co.user_import_dirs <- s :: co.user_import_dirs),
     " specify modules directory");
    ("--system-default-link-option",
     Arg.String (fun s -> co.system_default_link_option <- s),
     " system default link option");
  ] in
  Arg.parse speclist (fun s -> (co.input_files <- s :: co.input_files)) usagemsg;

  let system_libs_dirs = [
    co.system_import_dir_core;
    co.system_import_dir_std;
  ] @ List.rev co.system_import_dirs in

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
  let sem_ast = Sema.analyze_module m ctx in

  Printf.printf "===== PHASE = CODEGEN\n";
  flush_all ();

  let module Codegen = Codegen_llvm in

  let code_ctx =
    Codegen.make_default_context ~type_sets:ctx.Sema.sc_tsets
                                 ~uni_map:ctx.Sema.sc_unification_ctx
  in
  Codegen.generate sem_ast code_ctx;
  Codegen.create_executable code_ctx co.system_default_link_option "a.out";

  Printf.printf "===== PHASE = FINISHED\n";
  flush_all ();

  exit 0
