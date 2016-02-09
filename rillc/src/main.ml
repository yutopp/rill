let () =
  Printexc.record_backtrace true;

  let system_libs_dirs = [
    "./corelib/src";
    "./stdlib/src"
  ] in

  let module_search_dirs = [
  ] in
  let cur_dir = Sys.getcwd () in
  let module_search_dirs = cur_dir :: module_search_dirs in

  (* TODO: fix *)
  let filename = Filename.concat cur_dir "test/input0.rill" in

  Printf.printf "===== PHASE = ANALYZE SEMANTICS\n";
  flush_all ();
  let (env, ctx) = Sema.make_default_state system_libs_dirs module_search_dirs in
  let m = Sema.load_module_by_filepath filename ctx in
  let sem_ast = Sema.analyze_module m ctx in

  Printf.printf "===== PHASE = CODEGEN\n";
  flush_all ();

  let module M = (Codegen_llvm : Codegen.GENERATOR_TYPE) in

  let c_ctx = M.generate sem_ast in
  let tmp_stdlib_path = "./stdlib/lib/rillstd-rt.a" in (* TODO: fix *)
  M.create_executable c_ctx tmp_stdlib_path "a.out";

  Printf.printf "===== PHASE = FINISHED\n";
  flush_all ();
