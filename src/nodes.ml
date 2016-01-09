module type NodeContextType = sig
    type t
    type prev_ast_t
  end

module Make =
  functor (Ctx : NodeContextType) ->
    struct
      type ast =
          Module of ast * Ctx.t

        (* statements *)
        | StatementList of ast list
        | ExprStmt of ast
        | FunctionDefStmt of string * int list * ast * Ctx.t
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
        | PrevPassNode of Ctx.prev_ast_t

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
