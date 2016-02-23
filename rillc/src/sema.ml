(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets

include Sema_predef
include Sema_forward_ref
include Sema_construct_env

let make_default_env () =
  Env.make_root_env ()

let make_default_context root_env module_search_dirs =
    let type_gen = Type.Generator.default () in

  let create_builtin_class name inner_name =
    let env = Env.create_env root_env (
                               Env.Class (Env.empty_lookup_table ~init:0 (),
                                          {
                                            Env.cls_name = name;
                                            Env.cls_mangled = None;
                                            Env.cls_template_vals = [];
                                            Env.cls_detail = Env.ClsUndef;
                                          })
                             ) in
    let node = TAst.ExternClassDefStmt (name, inner_name, Some env) in
    complete_env env node;
    env
  in
  let register_builtin_type name inner_name =
    let id_name = Nodes.Pure name in
    let cenv = create_builtin_class id_name inner_name in
    Env.add_inner_env root_env name cenv;

    Type.Generator.generate_type type_gen
                                 (Type.UniqueTy cenv)
                                 []
                                 {
                                   Type_attr.ta_ref_val = Type_attr.Val;
                                   Type_attr.ta_mut = Type_attr.Immutable;
                                 }
  in

  let open Builtin_info in
  let tsets = {
    ts_type_gen = type_gen;
    ts_type_type = register_builtin_type type_type_i.external_name
                                         type_type_i.internal_name;

    ts_void_type_holder = ref Type.undef_ty;
    ts_bool_type_holder = ref Type.undef_ty;
    ts_int32_type_holder = ref Type.undef_ty;
    ts_array_type_holder = ref Type.undef_ty;
  } in

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
  } in

  (**)
  let builtin_mod_e = load_module ["core"] "builtin" ctx in
  ctx.sc_builtin_m_env <- Some builtin_mod_e;

  (* cache void type *)
  cache_builtin_type_info tsets.ts_void_type_holder
                          void_type_i.external_name
                          ctx;

  (* cache bool type *)
  cache_builtin_type_info tsets.ts_bool_type_holder
                          bool_type_i.external_name
                          ctx;

  (* cache int type *)
  cache_builtin_type_info tsets.ts_int32_type_holder
                          int32_type_i.external_name
                          ctx;

  (* cache array type *)
  cache_builtin_type_info tsets.ts_array_type_holder
                          array_type_i.external_name
                          ctx;
  ctx


let make_default_state system_libs_dirs user_srcs_dirs =
  let module_search_dirs = system_libs_dirs @ user_srcs_dirs in

  let env = make_default_env () in
  let ctx = make_default_context env module_search_dirs in
  (env, ctx)


let analyze_module mod_env ctx =
  let mod_node = Option.get mod_env.Env.rel_node in
  construct_env mod_node ctx.sc_root_env ctx None
