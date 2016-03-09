(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type compile_options_t = {
  mutable input_files   : string list;
  mutable output_file   : string;
}

let empty () =
  {
    input_files = [];
    output_file = "a.out";
  }


let () =
  (* debug option *)
  Printexc.record_backtrace true;

  (* Compile Option *)
  let co = empty() in

  let usagemsg = "Usage: semicaml [filename] <options>\n"; in
  let speclist = [
    ("-o", Arg.String (fun s -> co.output_file <- s), " specify output file name");
  ] in
  Arg.parse speclist (fun s -> (co.input_files <- s :: co.input_files)) usagemsg;
  List.print (String.print) stdout co.input_files;

  assert (List.length co.input_files = 1);

  let system_libs_dirs = [
    "./corelib/src";
    "./stdlib/src"
  ] in

  let module_search_dirs = [
  ] in
  let cur_dir = Sys.getcwd () in
  let module_search_dirs = cur_dir :: module_search_dirs in

  (* TODO: fix *)
  let filename = Filename.concat cur_dir (List.hd co.input_files) in

  Printf.printf "===== PHASE = ANALYZE SEMANTICS\n";
  flush_all ();
  let (env, ctx) = Sema.make_default_state system_libs_dirs module_search_dirs in
  let m = Sema.load_module_by_filepath filename ctx in
  let sem_ast = Sema.analyze_module m ctx in

  Printf.printf "===== PHASE = CODEGEN\n";
  flush_all ();

  let module Codegen = Codegen_llvm in

  let code_ctx =
    Codegen.make_default_context ~opt_uni_map:(Some ctx.Sema.sc_unification_ctx)
                                 ()
  in
  Codegen.generate sem_ast code_ctx;
  let tmp_stdlib_path = "./stdlib/lib/rillstd-rt.a" in (* TODO: fix *)
  Codegen.create_executable code_ctx tmp_stdlib_path "a.out";

  Printf.printf "===== PHASE = FINISHED\n";
  flush_all ();
