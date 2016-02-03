module type CONTEXT_TYPE =
  sig
    type ir_context_t
    type ir_builder_t
    type ir_module_t

    type ir_value_t
    type ir_type_t

    type 'ctx builtin_f_t
  end

module Context =
  struct
    module Make (Cgt : CONTEXT_TYPE) =
      struct
        module IdOrderedType =
          struct
            type t = Env.id_t
            let compare = Num.compare_num
          end
        module IdSet = Set.Make(IdOrderedType)

        type t = {
          mutable ir_context        : Cgt.ir_context_t;
          mutable ir_builder        : Cgt.ir_builder_t;
          mutable ir_module         : Cgt.ir_module_t;

          env_to_val_tbl    : (Env.id_t, Cgt.ir_value_t) Hashtbl.t;
          env_to_type_tbl   : (Env.id_t, Cgt.ir_type_t) Hashtbl.t;

          name_to_builtin_type_tbl  : (string, Cgt.ir_type_t) Hashtbl.t;
          name_to_builtin_func_tbl  : (string, t Cgt.builtin_f_t) Hashtbl.t;

          mutable defined_env       : IdSet.t
        }

        let init ~ir_context ~ir_builder ~ir_module =
          {
            ir_context = ir_context;
            ir_builder = ir_builder;
            ir_module = ir_module;

            env_to_val_tbl = Hashtbl.create 64;
            env_to_type_tbl = Hashtbl.create 64;

            name_to_builtin_type_tbl = Hashtbl.create 32;
            name_to_builtin_func_tbl = Hashtbl.create 32;

            defined_env = IdSet.empty;
          }


        let mark_env_as_defined ctx env =
          ctx.defined_env <- (IdSet.add env.Env.env_id ctx.defined_env)

        let is_env_defined ctx env =
          IdSet.mem env.Env.env_id ctx.defined_env


        let bind_env_to_val ctx env value =
          Hashtbl.add ctx.env_to_val_tbl env.Env.env_id value

        let find_val_from_env ctx env =
          Hashtbl.find ctx.env_to_val_tbl env.Env.env_id


        let bind_env_to_type ctx env ty =
          Hashtbl.add ctx.env_to_type_tbl env.Env.env_id ty

        let find_type_from_env ctx env =
          Hashtbl.find ctx.env_to_type_tbl env.Env.env_id


        let find_builtin_type ctx name =
          Hashtbl.find ctx.name_to_builtin_type_tbl name

        let bind_builtin_type ctx name f =
          Hashtbl.add ctx.name_to_builtin_type_tbl name f


        let find_builtin_func ctx name =
          Hashtbl.find ctx.name_to_builtin_func_tbl name

        let bind_builtin_func ctx name f =
          Hashtbl.add ctx.name_to_builtin_func_tbl name f
      end
  end


module TAst = Tagged_ast
module type GENERATOR_TYPE =
  sig
    type ctx_t

    val make_default_context : unit -> ctx_t

    val generate : TAst.ast -> ctx_t
    val create_executable : ctx_t -> string -> string -> unit
  end
