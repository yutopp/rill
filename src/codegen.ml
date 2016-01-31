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
        type 'id_t t = {
          ir_context        : Cgt.ir_context_t;
          ir_builder        : Cgt.ir_builder_t;
          ir_module         : Cgt.ir_module_t;

          env_to_val_tbl    : ('id_t, Cgt.ir_value_t) Hashtbl.t;
          env_to_type_tbl   : ('id_t, Cgt.ir_type_t) Hashtbl.t;
          name_to_builtin_f_tbl   : (string, ('id_t t) Cgt.builtin_f_t) Hashtbl.t;
        }

        let init ~ir_context ~ir_builder ~ir_module = {
          ir_context = ir_context;
          ir_builder = ir_builder;
          ir_module = ir_module;

          env_to_val_tbl = Hashtbl.create 64;
          env_to_type_tbl = Hashtbl.create 64;
          name_to_builtin_f_tbl = Hashtbl.create 64;
        }


        let bind_env_to_val ctx env value =
          Hashtbl.add ctx.env_to_val_tbl env.Env.env_id value

        let find_val_from_env ctx env =
          Hashtbl.find ctx.env_to_val_tbl env.Env.env_id


        let bind_env_to_type ctx env ty =
          Hashtbl.add ctx.env_to_type_tbl env.Env.env_id ty


        let find_builtin_func ctx name =
          Hashtbl.find ctx.name_to_builtin_f_tbl name

        let bind_builtin_func ctx name f =
          Hashtbl.add ctx.name_to_builtin_f_tbl name f
      end
  end


module type GENERATOR_TYPE =
  sig
    type ctx_t

    val generate : Sema.TaggedAst.t -> ctx_t
    val create_executable : ctx_t -> string -> string -> unit
  end
