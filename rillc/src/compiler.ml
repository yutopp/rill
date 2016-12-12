(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type compile_options_t = {
    mutable input_files         : string list;
    mutable output_file         : string option;

    mutable system_import_dirs  : string list;
    mutable user_import_dirs    : string list;

    mutable options             : string list;
    mutable no_corelib          : bool;
    mutable no_stdlib           : bool;

    mutable compile_only        : bool;
  }

let compile co filepath =
  let system_libs_dirs = List.rev co.system_import_dirs in

  let module_search_dirs = List.rev co.user_import_dirs in
  let cur_dir = Sys.getcwd () in
  let module_search_dirs = cur_dir :: module_search_dirs in

  Debug.printf "===== PHASE = ANALYZE SEMANTICS\n";
  let (env, ctx) = Sema.make_default_state system_libs_dirs module_search_dirs in
  let mod_env =
    match Sema.load_module filepath env ctx with
    | Some mod_env -> mod_env
    | None ->
       Printf.printf "Semantics error\n";
       exit 1
  in
  let sem_ast = Option.get mod_env.Env.rel_node in

  Debug.printf "===== PHASE = CODEGEN\n";
  let code_ctx =
    Codegen_llvm.make_default_context ~type_sets:ctx.Sema.sc_tsets
                                      ~uni_map:ctx.Sema.sc_unification_ctx
                                      ~target_module:(Some mod_env)
  in
  let object_filepath =
    match co.output_file with
    | Some name -> name
    | None ->
       try
         (Filename.chop_extension filepath) ^ ".o"
       with
       | Invalid_argument _ -> filepath
  in
  Codegen_llvm.create_object sem_ast object_filepath code_ctx
