type id_string =
    Pure of string
  | UnaryPreOp of string
  | UnaryPostOp of string
  | BinaryOp of string

let string_of_binary_op s =
  "op_binary_" ^ s

let string_of_id_string id_s =
  match id_s with
  | Pure s -> s
  | UnaryPreOp s -> "op_unary_pre_" ^ s
  | UnaryPostOp s -> "op_unary_post_" ^ s
  | BinaryOp s -> string_of_binary_op s


module type NodeContextType =
  sig
    type 'a current_ctx_t
    type 'a prev_ctx_t
  end

module Make (Ctx : NodeContextType) =
  struct
    type ast =
        Module of ast * ctx_t

      (*
       * statements
       *)
      | StatementList of ast list
      | ExprStmt of ast
      (* name, params, return_type, body, _ *)
      | FunctionDefStmt of id_string * ast * ast option * ast * ctx_t
      (* name, params, return_type, function name(TODO: change to AST), _ *)
      | ExternFunctionDefStmt of id_string * ast * ast * string * ctx_t
      | VariableDefStmt of Type.Attr.ref_val * ast * ctx_t (* ref/val, init, _ *)
      | EmptyStmt
      | AttrWrapperStmt of (string, ast option) Hashtbl.t * ast

      (*
       * expressions
       *)
      | BinaryOpExpr of ast * id_string * ast
      | UnaryOpExpr of string * ast

      | ElementSelectionExpr of ast * ast
      | SubscriptingExpr of ast * ast option
      | CallExpr of ast * ast list

      (*
       * values
       *)
      | Id of id_string * ctx_t
      | Int32Lit of int
      | StringLit of string
      | BoolLit of bool

      (* error *)
      | Error

      (* special *)
      | ParamsList of param_init_t list
      | VarInit of var_init_t
      | PrevPassNode of pctx_t

      | GenericCall of string * ast list * ctx_t


     (* id * value *)
     and param_init_t = string option * value_init_t
     (* *)
     and var_init_t = string * value_init_t

     (* type * default value *)
     and value_init_t = ast option * ast option

     and ctx_t = ast Ctx.current_ctx_t
     and pctx_t = ast Ctx.prev_ctx_t


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

      | ExprStmt _ ->
         print_string "ExprStmt\n"

      | FunctionDefStmt (id, _, _, statements, ctx) ->
         begin
           open_hbox();
           print_string "function def : "; print_string (string_of_id_string id); print_string "\n";
           print statements;
           close_box()
         end

      | ExternFunctionDefStmt _ ->
         print_string "ExternFunctionDefStmt\n"

      | VariableDefStmt _ ->
         print_string "VariableDefStmt\n"

      | EmptyStmt ->
         begin
           open_hbox();
           print_string "EMPTY";
           close_box()
         end

      | _ ->
         print_string "unknown\n"
  end
