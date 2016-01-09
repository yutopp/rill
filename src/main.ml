let () =
  let ast = Syntax.make_ast_from_file "test/input0.rill" in
  Ast.print ast;
  let env = Sema.make_default_env () in
  let sem_ast = Sema.analyze ast env in
  let module M = (Llvm_codegen : Codegen.GENERATOR_TYPE) in
  let c_ctx = M.generate sem_ast in
  M.create_executable c_ctx "" "a.out"
