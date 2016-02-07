let () =
  Printexc.record_backtrace true;

  let ast = Syntax.make_ast_from_file "test/input0.rill" in
  Ast.print ast;

  Printf.printf "===== PHASE = ANALYZE SEMANTICS\n";
  flush_all ();

  let (env, ctx) = Sema.make_default_state () in
  let sem_ast = Sema.analyze ast env ctx in

  Printf.printf "===== PHASE = CODEGEN\n";
  flush_all ();

  let module M = (Codegen_llvm : Codegen.GENERATOR_TYPE) in

  let c_ctx = M.generate sem_ast in
  let tmp_stdlib_path = "./stdlib/lib/rillstd-rt.a" in (* TODO: fix *)
  M.create_executable c_ctx tmp_stdlib_path "a.out";

  Printf.printf "===== PHASE = FINISHED\n";
  flush_all ();
