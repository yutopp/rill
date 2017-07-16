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
  (* input filepaths for compilation *)
  mutable input_files                       : string list;
  (* output filepath *)
  mutable output_file                       : string option;
  (* if true, do only compilation *)
  mutable compile_only                      : bool;
  (* format of object *)
  mutable object_format                     : Codegen_format.t;

  (* if true, ignore default system import dirs *)
  mutable ignore_default_system_import_dirs : bool;
  (* use these dirs to lookup rill packages *)
  mutable system_import_dirs                : string list;
  (* use these dirs to lookup user packages *)
  mutable user_import_dirs                  : string list;

  (* if true, corelib will be not linked *)
  mutable no_link_corelib                   : bool;
  (* if true, stdlib will be not linked *)
  mutable no_link_stdlib                    : bool;

  mutable target_triple                     : string;
  mutable options                           : COS.t list;
}

let default_options () =
  {
    input_files = [];
    output_file = None;
    compile_only = false;
    object_format = Codegen_format.OfObject;

    ignore_default_system_import_dirs = false;
    system_import_dirs = [];
    user_import_dirs = [];

    no_link_corelib = false;
    no_link_stdlib = false;

    target_triple = "x86_64-unknown-linux-gnu"; (* TODO: read a default value from config *)

    options = [];
  }

module Arg =
  struct
    let usagemsg = "Usage: rillc <options> [filename]\n"

    let show_version_and_exit () =
      Printf.printf "rillc %s\n%!" Config.version;
      exit 0

    let speclist co = [
      ("-o",
       Arg.String (fun s -> co.output_file <- Some s),
       "<path> specify an output file name");
      ("--ignore-default-system-import",
       Arg.Unit (fun b -> co.ignore_default_system_import_dirs <- true),
       " Ignore default system import dirs");
      ("--system-import-dir",
       Arg.String (fun s -> co.system_import_dirs <- s :: co.system_import_dirs),
       "<dir> Append a directory as system import dir");
      ("--system-lib",
       Arg.String (fun s -> co.system_import_dirs <- s :: co.system_import_dirs),
       "<dir> [Deprecated: use '--system-import-dir' instead] Append a directory as system import dir");
      ("-I",
       Arg.String (fun s -> co.user_import_dirs <- s :: co.user_import_dirs),
       "<dir> Append a directory as import dir");
      ("-L",
       Arg.String (fun s -> co.options <- (COS.OsLinkDir s) :: co.options),
       "<option> Linker option");
      ("-l",
       Arg.String (fun s -> co.options <- (COS.OsLinkLib s) :: co.options),
       "<option> Linker option");
      ("--target",
       Arg.String (fun s -> co.target_triple <- s),
       "<target> Specify target triple");
      ("--no-link-corelib",
       Arg.Unit (fun () -> co.no_link_corelib <- true),
       " Do not link corelib");
      ("--no-corelib",
       Arg.Unit (fun () -> co.no_link_corelib <- true),
       " [Deprecated: use '--no-link-corelib' instead] Do not link corelib");
      ("--no-link-stdlib",
       Arg.Unit (fun () -> co.no_link_stdlib <- true),
       " Do not link stdlib");
      ("--no-stdlib",
       Arg.Unit (fun () -> co.no_link_stdlib <- true),
       " [Deprecated: use '--no-link-stdlib' instead] Do not link stdlib");
      ("-c",
       Arg.Unit (fun () -> co.compile_only <- true),
       " Compile only");
      ("--format",
       Arg.String (fun s -> co.object_format <- (Codegen_format.of_string s)),
       " Output format (only compile mode)");
      ("--version",
       Arg.Unit show_version_and_exit,
       " Show version");
    ]

    let validate_options co =
      if co.compile_only && (List.length co.input_files > 1) && Option.is_some co.output_file then
        begin
          Printf.eprintf "cannot specify -o option when multiple files and -c option are given";
          exit 1
        end;

      if List.length co.input_files = 0 then
        begin
          Printf.eprintf "%s" usagemsg;
          exit 1
        end;

      (* update system_import_dirs *)
      let co =
        (* the left is priority *)
        let system_import_dirs =
          (co.system_import_dirs |> List.rev)
          @ if co.ignore_default_system_import_dirs then
              []
            else
              if Config.use_local_dev_lib then
                (* for debugging *)
                ["./corelib/src"; "./stdlib/src"]
              else
                Config.default_system_import_dirs
        in
        {co with system_import_dirs}
      in

      (* update user_import_dirs *)
      let co =
        let user_import_dirs = co.user_import_dirs |> List.rev in
        {co with user_import_dirs}
      in

      co

    let parse () =
      (* Compile Option *)
      let co = default_options () in
      Arg.parse (speclist co |> Arg.align)
                (fun s -> co.input_files <- s :: co.input_files)
                usagemsg;

      validate_options co
  end

let to_fullpath path =
  let cur_dir = Sys.getcwd () in
  if Filename.is_relative path then
    Filename.concat cur_dir path
  else
    path

let compile co build_options in_filepath out_filepath =
  let system_libs_dirs =
    co.system_import_dirs |> List.map to_fullpath
  in

  let module_search_dirs =
    Sys.getcwd () :: co.user_import_dirs |> List.map to_fullpath
  in

  let (env, ctx) =
    Sema.make_default_state system_libs_dirs
                            module_search_dirs
                            build_options (* for dynamic linking in ctfe *)
  in
  match Sema.load_module (to_fullpath in_filepath) env ctx with
  | Some mod_env ->
     Debug.reportf "===== PHASE = CODEGEN\n";
     Codegen_llvm.emit ~type_sets:ctx.Sema.Context.sc_tsets
                       ~uni_map:ctx.Sema.Context.sc_unification_ctx
                       ~target_module:mod_env
                       co.target_triple
                       co.object_format
                       (to_fullpath out_filepath)

  | None ->
     (* TODO *)
     Bad "failed to load module"

let make_build_options co =
  let core_lib_opts = if co.no_link_corelib then
                        []
                      else
                        if Config.use_local_dev_lib then
                          [COS.OsLinkDir "./corelib/lib";
                           COS.OsLinkLib "rillcore-rt"]
                        else
                          [COS.OsLinkDir Config.default_core_lib_dir;
                           COS.OsLinkLib Config.default_core_lib_name]
  in
  let std_lib_opts = if co.no_link_stdlib then
                       []
                     else
                       if Config.use_local_dev_lib then
                         [COS.OsLinkDir "./stdlib/lib";
                          COS.OsLinkLib "rillstd-rt"]
                       else
                         [COS.OsLinkDir Config.default_std_lib_dir;
                          COS.OsLinkLib Config.default_std_lib_name]
  in
  co.options @ std_lib_opts @ core_lib_opts

let to_outfile base_dir path format =
  let base =
    let basename = Filename.basename path in
    match Filename.chop_extension basename with
    | s -> s
    | exception Invalid_argument _ -> basename
  in
  let ext =
    match format with
    | Codegen_format.OfExecutable -> ""
    | Codegen_format.OfAssembly -> ".s"
    | Codegen_format.OfObject -> ".o"
    | Codegen_format.OfLlvm -> ".ll"
  in
  (Filename.concat base_dir base) ^ ext

let build co =
  let build_options = make_build_options co in

  let compile_io_pairs =
    match (co.compile_only, co.input_files, co.output_file) with
    | (true, [filepath], Some out_filepath) ->
       [(filepath, out_filepath)];
    | (true, filepaths, None) ->
       filepaths |> List.map (fun s -> (s, to_outfile "." s co.object_format))
    | (true, _, _) ->
       failwith "[ERR] not supported options"
    | (false, filepaths, _) ->
       filepaths |> List.map (fun s -> (s, to_outfile "." s co.object_format))
  in

  let res_obj_file_names =
    compile_io_pairs
    |> List.map (fun (input, output) -> compile co build_options input output)
  in

  let (obj_file_names, errors) =
    List.fold_right (fun res_obj_file_name (acc_ok, acc_err) ->
                     match res_obj_file_name with
                     | Ok obj_file_name ->
                        (obj_file_name :: acc_ok, acc_err)
                     | Bad reason ->
                        (acc_ok, reason :: acc_err)
                    ) res_obj_file_names ([], [])
  in
  match (co.compile_only, obj_file_names, errors) with
  | (true, obj_file_names, []) ->
     Ok ""
  | (false, obj_file_names, []) ->
     (* link required *)
     let () =
       let out_filepath =
         match co.output_file with
         | Some s ->
            s
         | None ->
            to_outfile "." (List.hd obj_file_names) Codegen_format.OfExecutable
       in
       Codegen_executable.link_objects obj_file_names build_options
                                       (to_fullpath out_filepath)
     in
     Ok ""
  | (_, _, errors) ->
     Bad errors
