(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module type CONTEXT_TYPE =
  sig
    type ir_context_t
    type ir_builder_t
    type ir_module_t
    type ir_intrinsics

    type ('ty, 'ctx) value_record_t
  end

module Make (Cgt : CONTEXT_TYPE) =
  struct
    module EnvIdOrderedType =
      struct
        type t = Env_system.EnvId.t
        let compare = Env_system.EnvId.compare
      end
    module IdSet = Set.Make(EnvIdOrderedType)

    type ('env, 'ty, 'v) t = {
      mutable ir_context        : Cgt.ir_context_t;
      mutable ir_builder        : Cgt.ir_builder_t;
      mutable ir_module         : Cgt.ir_module_t;
      intrinsics                : Cgt.ir_intrinsics;

      env_to_record_tbl         : (EnvIdOrderedType.t, ('env, 'ty, 'v) value_t) Hashtbl.t;
      name_to_record_tbl        : (string, ('env, 'ty, 'v) value_t) Hashtbl.t;

      env_to_meta_record_tbl    : (EnvIdOrderedType.t, ('env, 'ty, 'v) value_t) Hashtbl.t;

      mutable defined_env       : IdSet.t;
      type_sets                 : 'env Type_sets.type_sets_t;
      uni_map                   : ('ty, 'v) Unification.t;

      places_for_sto_array_elem : ('env, 'ty, 'v) value_t Stack.t;
    }
     and ('env, 'ty, 'v) value_t = ('ty, (('env, 'ty, 'v) t)) Cgt.value_record_t

    let init ~ir_context ~ir_builder ~ir_module ~ir_intrinsics
             ~type_sets ~uni_map =
      {
        ir_context = ir_context;
        ir_builder = ir_builder;
        ir_module = ir_module;
        intrinsics = ir_intrinsics;

        env_to_record_tbl = Hashtbl.create 32;
        name_to_record_tbl = Hashtbl.create 32;

        env_to_meta_record_tbl = Hashtbl.create 32;

        defined_env = IdSet.empty;
        type_sets = type_sets;
        uni_map = uni_map;

        places_for_sto_array_elem = Stack.create ();
      }

    (**)
    let mark_env_as_defined ctx env =
      ctx.defined_env <- (IdSet.add env.Env.env_id ctx.defined_env)

    let is_env_defined ctx env =
      IdSet.mem env.Env.env_id ctx.defined_env


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
    let bind_metaval_to_env ctx value env =
      Hashtbl.add ctx.env_to_meta_record_tbl env.Env.env_id value

    let find_metaval_by_env ctx env =
      Hashtbl.find ctx.env_to_meta_record_tbl env.Env.env_id


    (**)
    let push_array_storage ctx array_value =
      Stack.push array_value ctx.places_for_sto_array_elem

    let pop_array_storage ctx =
      Stack.pop ctx.places_for_sto_array_elem

    let current_array_storage ctx =
      Stack.top ctx.places_for_sto_array_elem
  end
