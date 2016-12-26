(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module type CONTEXT_TYPE =
  sig
    type ir_context_t
    type ir_builder_t
    type ir_module_t
    type ir_value_t

    type ir_intrinsics
    type 'ty ir_cache_value_t

    type ('ty, 'ctx) value_t
  end

module Make (Cgt : CONTEXT_TYPE) =
  struct
    module EnvIdOrd =
      struct
        type t = Env_system.EnvId.t
        let compare = Env_system.EnvId.compare
      end
    module EnvIdSet = Set.Make(EnvIdOrd)

    module NodeOrderedType =
      struct
        type t = Env_system.EnvId.t
        let compare = Env_system.EnvId.compare
      end

    type ('env, 'c_id, 'ty, 'v) t = {
      mutable ir_context        : Cgt.ir_context_t;
      mutable ir_builder        : Cgt.ir_builder_t;
      mutable ir_module         : Cgt.ir_module_t;
      intrinsics                : Cgt.ir_intrinsics;

      env_to_record_tbl         : (EnvIdOrd.t, ('env, 'c_id, 'ty, 'v) value_t) Hashtbl.t;
      name_to_record_tbl        : (string, ('env, 'c_id, 'ty, 'v) value_t) Hashtbl.t;


      env_to_meta_record_tbl    : (EnvIdOrd.t, ('env, 'c_id, 'ty, 'v) value_t) Hashtbl.t;
      cache_id_to_cache_tbl     : ('c_id, 'ty Cgt.ir_cache_value_t) Hashtbl.t;

      mutable defined_env       : EnvIdSet.t;
      type_sets                 : 'env Type_sets.type_sets_t;
      uni_map                   : ('ty, 'v) Unification.t;
      target_module_id          : Env_system.EnvId.t option;

      external_functions        : (string, Cgt.ir_value_t) Hashtbl.t;

      places_for_sto_array_elem : ('env, 'c_id, 'ty, 'v) value_t Stack.t;
    }
     and ('env, 'c_id, 'ty, 'v) value_t =
       ('ty, (('env, 'c_id, 'ty, 'v) t)) Cgt.value_t

    let init ~ir_context ~ir_builder ~ir_module ~ir_intrinsics
             ~type_sets ~uni_map ~target_module_id =
      {
        ir_context = ir_context;
        ir_builder = ir_builder;
        ir_module = ir_module;
        intrinsics = ir_intrinsics;

        env_to_record_tbl = Hashtbl.create 32;
        name_to_record_tbl = Hashtbl.create 32;

        env_to_meta_record_tbl = Hashtbl.create 32;
        cache_id_to_cache_tbl = Hashtbl.create 32;

        defined_env = EnvIdSet.empty;
        type_sets = type_sets;
        uni_map = uni_map;
        target_module_id = target_module_id;

        external_functions = Hashtbl.create 32;

        places_for_sto_array_elem = Stack.create ();
      }

    (**)
    let mark_env_as_defined ctx env =
      ctx.defined_env <- (EnvIdSet.add env.Env.env_id ctx.defined_env)

    let is_env_defined ctx env =
      EnvIdSet.mem env.Env.env_id ctx.defined_env


    (**)
    let bind_val_to_env ctx value env =
      Hashtbl.add ctx.env_to_record_tbl env.Env.env_id value

    let find_val_by_env ctx env =
      Hashtbl.find ctx.env_to_record_tbl env.Env.env_id


    (**)
    let bind_val_to_name ctx value name =
      Hashtbl.add ctx.name_to_record_tbl name value

    let find_val_by_name ctx name =
      Hashtbl.find ctx.name_to_record_tbl name


    (**)
    let bind_external_function ctx name f =
      Hashtbl.add ctx.external_functions name f

    let find_external_function_by_name ctx name =
      Hashtbl.find ctx.external_functions name

    let enum_of_external_function_names ctx =
      Hashtbl.keys ctx.external_functions


    (**)
    let bind_metaval_to_env ctx value env =
      Hashtbl.add ctx.env_to_meta_record_tbl env.Env.env_id value

    let find_metaval_by_env ctx env =
      Hashtbl.find ctx.env_to_meta_record_tbl env.Env.env_id

    (**)
    let bind_values_to_cache_id ctx values cache_id  =
      Hashtbl.add ctx.cache_id_to_cache_tbl cache_id values

    let find_values_by_cache_id ctx cache_id =
      Hashtbl.find ctx.cache_id_to_cache_tbl cache_id

    (**)
    let push_array_storage ctx array_value =
      Stack.push array_value ctx.places_for_sto_array_elem

    let pop_array_storage ctx =
      Stack.pop ctx.places_for_sto_array_elem

    let current_array_storage ctx =
      Stack.top ctx.places_for_sto_array_elem
  end
