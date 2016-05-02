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

  let register_builtin_type name inner_name meta_level size align =
    let create_builtin_class name inner_name =
      let env_r = Env.ClassOp.empty_record name in
      env_r.Env.cls_mangled <- Some inner_name;
      env_r.Env.cls_size <- size;
      env_r.Env.cls_align <- align;

      let env = Env.create_context_env root_env (
                                         Env.Class (Env.empty_lookup_table ~init:0 (),
                                                    env_r)
                                       ) in
      env.Env.meta_level <- meta_level;

      let node = TAst.ExternClassDefStmt (name, inner_name, None, Some env) in

      let detail_r = Env.ClsRecordPrimitive {
                         Env.cls_e_name = inner_name;
                       } in
      let traits = {
        Env.cls_traits_is_primitive = true;
      } in
      complete_class_env env node detail_r traits;
      env
    in

    let id_name = Nodes.Pure name in
    let cenv = create_builtin_class id_name inner_name in
    Env.add_inner_env root_env name cenv;

    Type.Generator.generate_type type_gen
                                 (Type_info.UniqueTy cenv)
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
                                         type_type_i.internal_name
                                         Meta_level.OnlyMeta
                                         8 8; (* TODO: fix *)
    ts_void_type = register_builtin_type void_type_i.external_name
                                         void_type_i.internal_name
                                         Meta_level.Meta
                                         0 0;

    ts_bool_type_holder = ref Type_info.undef_ty;
    ts_uint8_type_holder = ref Type_info.undef_ty;
    ts_int32_type_holder = ref Type_info.undef_ty;
    ts_array_type_holder = ref Type_info.undef_ty;
    ts_untyped_raw_ptr_type_holder = ref Type_info.undef_ty;
    ts_raw_ptr_type_holder = ref Type_info.undef_ty;
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

  (* cache bool type *)
  cache_builtin_type_info tsets.ts_bool_type_holder
                          bool_type_i.external_name
                          (Some (1, 1)) (* TODO: fix *)
                          ctx;

  (* cache uint8 type *)
  cache_builtin_type_info tsets.ts_uint8_type_holder
                          uint8_type_i.external_name
                          (Some (1, 1)) (* TODO: fix *)
                          ctx;

  (* cache int32 type *)
  cache_builtin_type_info tsets.ts_int32_type_holder
                          int32_type_i.external_name
                          (Some (4, 4)) (* TODO: fix *)
                          ctx;

  (* cache array type *)
  cache_builtin_type_info tsets.ts_array_type_holder
                          array_type_i.external_name
                          None
                          ctx;

  (* cache pointer types *)
  cache_builtin_type_info tsets.ts_untyped_raw_ptr_type_holder
                          untyped_raw_ptr_type_i.external_name
                          (Some (8, 8)) (* TODO: fix *)
                          ctx;

  cache_builtin_type_info tsets.ts_raw_ptr_type_holder
                          raw_ptr_type_i.external_name
                          None
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
  node
