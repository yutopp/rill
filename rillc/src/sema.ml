(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets

include Sema_definitions
include Sema_forward_ref
include Sema_construct_env

let make_default_env () =
  Env.make_root_env ()

let make_default_context root_env module_search_dirs =
  let type_gen = Type.Generator.default () in

  let open Sema_utils in
  let open Builtin_info in

  let type_type =
    register_builtin_type type_type_i.external_name
                          type_type_i.internal_name
                          Meta_level.OnlyMeta
                          (Some (8, 8))  (* TODO: fix *)
                          root_env type_gen
  in
  let void_type =
    register_builtin_type void_type_i.external_name
                          void_type_i.internal_name
                          Meta_level.Meta
                          None          (* TODO: fix *)
                          root_env type_gen
  in
  let tsets = Type_sets.make type_gen type_type void_type in

  let uni_map = Unification.empty () in
  let ctfe_engine = Ctfe_engine.initialize tsets uni_map in
  let ctx = {
    sc_root_env = root_env;
    sc_builtin_m_env = None;

    sc_module_search_dirs = module_search_dirs;
    sc_module_bag = Module_info.Bag.empty ();

    sc_ctfe_engine = ctfe_engine;
    sc_tsets = tsets;
    sc_unification_ctx = uni_map;
    sc_errors = []
  } in

  (**)
  let builtin_mod_e = load_module ["core"] "builtin" ctx in
  ctx.sc_builtin_m_env <- Some builtin_mod_e;

  (* cache bool type *)
  cache_builtin_type_info tsets.ts_bool_type_holder
                          bool_type_i.external_name
                          ctx;

  (* cache uint8 type *)
  cache_builtin_type_info tsets.ts_uint8_type_holder
                          uint8_type_i.external_name
                          ctx;

  (* cache int32 type *)
  cache_builtin_type_info tsets.ts_int32_type_holder
                          int32_type_i.external_name
                          ctx;

  (* cache uint32 type *)
  cache_builtin_type_info tsets.ts_uint32_type_holder
                          uint32_type_i.external_name
                          ctx;

  (* cache array type *)
  cache_builtin_type_info tsets.ts_array_type_holder
                          array_type_i.external_name
                          ctx;

  (* cache pointer types *)
  cache_builtin_type_info tsets.ts_untyped_raw_ptr_type_holder
                          untyped_raw_ptr_type_i.external_name
                          ctx;

  cache_builtin_type_info tsets.ts_raw_ptr_type_holder
                          raw_ptr_type_i.external_name
                          ctx;
  ctx


let make_default_state system_libs_dirs user_srcs_dirs =
  let module_search_dirs = system_libs_dirs @ user_srcs_dirs in

  let env = make_default_env () in
  let ctx = make_default_context env module_search_dirs in
  (env, ctx)


let analyze_module mod_env ctx =
  let mod_node = Option.get mod_env.Env.rel_node in
  let (node, _) = construct_env mod_node ctx.sc_root_env ctx None in
  match List.length ctx.sc_errors with
  | 0 -> Some node
  | _ -> None
