(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
module COS = Codegen_option_spec

type compile_options_t = {
    mutable input_files         : string list;
    mutable output_file         : string option;

    mutable system_import_dirs  : string list;
    mutable user_import_dirs    : string list;

    mutable options             : COS.t list;
    mutable no_corelib          : bool;
    mutable no_stdlib           : bool;

    mutable compile_only        : bool;
  }

let to_fullpath path =
  let cur_dir = Sys.getcwd () in
  if Filename.is_relative path then
    Filename.concat cur_dir path
  else
    path

let make_build_options co =
  let core_lib_opts = if co.no_corelib then
                        []
                      else
                        if Config.use_local_dev_lib then
                          [COS.OsLinkDir "./corelib/lib";
                           COS.OsLinkLib "rillcore-rt"]
                        else
                          [COS.OsLinkDir Config.default_core_lib_dir;
                           COS.OsLinkLib Config.default_core_lib_name]
  in
  let std_lib_opts = if co.no_corelib then
                       []
                     else
                       if Config.use_local_dev_lib then
                         [COS.OsLinkDir "./stdlib/lib";
                          COS.OsLinkLib "rillstd-rt"]
                       else
                         [COS.OsLinkDir Config.default_std_lib_dir;
                          COS.OsLinkLib Config.default_std_lib_name]
  in
  core_lib_opts @ std_lib_opts @ co.options

let compile co build_options filepath' =
  let filepath = to_fullpath filepath' in

  let system_libs_dirs =
    List.rev co.system_import_dirs
    |> List.map to_fullpath
  in

  let module_search_dirs =
    Sys.getcwd () :: List.rev co.user_import_dirs
    |> List.map to_fullpath
  in

  let (env, ctx) =
    Sema.make_default_state system_libs_dirs module_search_dirs
                            build_options (* for dynamic linking in ctfe *)
  in
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
    Codegen_llvm.make_default_context ~type_sets:ctx.Sema.Context.sc_tsets
                                      ~uni_map:ctx.Sema.Context.sc_unification_ctx
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
