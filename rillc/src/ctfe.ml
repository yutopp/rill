module Make(CGen : Codegen.GENERATOR_TYPE) =
  struct
    type t = {
      ir_ctx: CGen.ctx_t;
    }

    let empty () =
      {
        ir_ctx = CGen.make_default_context ();
      }

    let execute engine node =
      ()

  end

(*


let empty () =
  {
    ir_ctx = Llvm_codegen.make_default_context ();
  }
 *)
