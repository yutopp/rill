module type CONTEXT = sig
    type ctx
  end

module PPi =
  functor (Ctx : CONTEXT) ->
    struct
      type ast =
          Module of ast * Ctx.ctx

        (* statements *)
        | StatementList of ast list
        | ExprStmt of ast
        | FunctionDefStmt of string * int list * ast
        | EmptyStmt

        (* expressions *)
        | BinaryOpExpr of ast * string * ast
        | UnaryOpExpr of string * ast

        (* values *)
        | Id of string
        | Int32Lit of int

        (* error *)
        | Error


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

        | FunctionDefStmt (id, _, statements) ->
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


module AstContext =
  struct
    type ctx = unit
  end

module Ast = PPi(AstContext)
