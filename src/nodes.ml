module type NodeContextType =
  sig
    type 'a t
    type 'a prev_ast_t
  end

module Make (Ctx : NodeContextType) =
  struct
    type ast =
        Module of ast * ctx_t

      (* statements *)
      | StatementList of ast list
      | ExprStmt of  ast
      | FunctionDefStmt of string * ast * ast * ctx_t (* name, params, body, _ *)
      | VariableDefStmt of Type.Attr.ref_val * ast (* ref/val, init *)
      | EmptyStmt

      (* expressions *)
      | BinaryOpExpr of ast * string * ast
      | UnaryOpExpr of string * ast

      (* values *)
      | Id of string
      | Int32Lit of int

      (* error *)
      | Error

      (* special *)
      | ParamsList of param_init_t list
      | VarInit of var_init_t
      | PrevPassNode of pctx_t

     (* id * value *)
     and param_init_t = string option * value_init_t
     (* *)
     and var_init_t = string * value_init_t

     (* type * default value *)
     and value_init_t = ast option * ast option


     and ctx_t = ast Ctx.t
     and pctx_t = ast Ctx.prev_ast_t

    type t = ast

    (* debug print *)
    let rec print ast =
      let open Format in
      match ast with
      | Module (a, ctx) ->
         begin
           open_hbox();
           print_string "module";
           print_newline();
           print a;
           close_box();
           print_newline ()
         end

      | StatementList asts ->
         begin
           asts |> List.iter (fun a -> print a; print_newline())
         end

      | FunctionDefStmt (id, _, statements, ctx) ->
         begin
           open_hbox();
           print_string "function def : "; print_string id; print_string "\n";
           print statements;
           close_box()
         end

      | EmptyStmt ->
         begin
           open_hbox();
           print_string "EMPTY";
           close_box()
         end

      | otherwise -> ()
  end
