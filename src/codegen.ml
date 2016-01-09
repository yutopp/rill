module type CONTEXT_TYPE =
  sig
    type ir_context_t
    type ir_builder_t
    type ir_module_t
  end

module Context =
  struct
    module Make (Cgt : CONTEXT_TYPE) =
      struct
        type t = {
          ir_context    : Cgt.ir_context_t;
          ir_builder    : Cgt.ir_builder_t;
          ir_module     : Cgt.ir_module_t
        }

        let init ~ir_context ~ir_builder ~ir_module = {
          ir_context = ir_context;
          ir_builder = ir_builder;
          ir_module = ir_module;
        }
      end
  end


module type GENERATOR_TYPE =
  sig
    type ctx_t
    val generate : Sema.TaggedAst.t -> ctx_t
    val create_executable : ctx_t -> string -> string -> unit
  end
