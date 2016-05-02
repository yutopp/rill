(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

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
    type 'a term_ctx_t
    type 'a prev_ctx_t
  end

module Make (Ctx : NodeContextType) =
  struct
    type ast =
        Module of ast * string list * string * string * ctx_t

      (*
       * statements
       *)
      | StatementList of ast list
      | ExprStmt of ast
      | ReturnStmt of ast option
      | ImportStmt of string list * string * ctx_t
      (* name, params, return_type?, instance_cond, body, attribute?, _ *)
      | FunctionDefStmt of id_string * ast * ast option * ast option * ast * attr_tbl_t option * ctx_t
      | MemberFunctionDefStmt of id_string * ast * ast option * ast * attr_tbl_t option * ctx_t
      (* name, params, return_type, function name(TODO: change to AST), attribute?, _ *)
      | ExternFunctionDefStmt of id_string * ast * Meta_level.t * ast * string * attr_tbl_t option * ctx_t
      (* name, body, attribute?, _ *)
      | ClassDefStmt of id_string * ast * attr_tbl_t option * ctx_t
      | ExternClassDefStmt of id_string * string * attr_tbl_t option * ctx_t
      (* VarInit, _ *)
      | VariableDefStmt of Meta_level.t * ast * ctx_t
      | MemberVariableDefStmt of ast * ctx_t
      (* name, template params, inner node *)
      | TemplateStmt of id_string * ast * ast
      | EmptyStmt
      | AttrWrapperStmt of (string, ast option) Hashtbl.t * ast

      (*
       * expressions
       *)
      | BinaryOpExpr of ast * id_string * ast
      | UnaryOpExpr of id_string * ast

      | ElementSelectionExpr of ast * ast * ctx_t
      | SubscriptingExpr of ast * ast option
      | CallExpr of ast * ast list
      | ScopeExpr of ast
      | IfExpr of ast * ast * ast option
      | ForExpr of ast option * ast option * ast option * ast
      | NewExpr of ast
      | DeleteExpr of ast
      | StatementTraitsExpr of string * ast

      (*
       * values
       *)
      | Id of id_string * ctx_t
      | InstantiatedId of id_string * ast list * ctx_t
      | Int32Lit of int * term_ctx_t
      | StringLit of string * term_ctx_t
      | BoolLit of bool * term_ctx_t
      | ArrayLit of ast list * term_ctx_t

      (* error *)
      | Error

      (* special *)
      | ParamsList of param_init_t list
      | TemplateParamsList of template_param_init_t list
      | VarInit of var_init_t
      | PrevPassNode of pctx_t
      | NotInstantiatedNode of pctx_t * attr_tbl_t option

      (* *)
      | GenericId of id_string * ctx_t
      (* object construction, args, ctx *)
      | GenericCallExpr of storage_t ref * ast list * ctx_t * ctx_t
      (* body, ctx *)
      | GenericFuncDef of ast option * ctx_t
      | NestedExpr of ast * term_aux_t * term_ctx_t * ctx_t


     and term_aux_t = (term_ctx_t * Value_category.t * Type_attr.lifetime_t * Meta_level.t)

     (* attr * id? * value *)
     and param_init_t = Type_attr.attr_t * string option * value_init_t
     (* id * value? *)
     and template_param_init_t = string * value_init_t option
     (* attr * id * value *)
     and var_init_t = var_aux_t * string * value_init_t
     and var_aux_t = Type_attr.attr_t

     (* type * default value *)
     and value_init_t = ast option * ast option

     and attr_tbl_t = (string, ast option) Hashtbl.t

     and ctx_t = ast Ctx.current_ctx_t
     and term_ctx_t = ast Ctx.term_ctx_t
     and pctx_t = ast Ctx.prev_ctx_t

     and storage_t =
       | StoStack of term_ctx_t
       | StoHeap
       | StoGc
       | StoAgg
       | StoImm

    type t = ast

    (* debug print *)
    let rec print ast =
      let open Format in
      match ast with
      | Module (a, _, _, _, ctx) ->
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

      | FunctionDefStmt (id, _, _, _, statements, _, ctx) ->
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


      | BinaryOpExpr (lhs, op, rhs) ->
         begin
           print lhs; print_string (string_of_id_string op); print rhs
         end

      | UnaryOpExpr (op, expr) ->
         begin
           print_string (string_of_id_string op); print expr
         end

      | ElementSelectionExpr (recv, sel, _) ->
         begin
           print recv; print_string "."; print sel
         end

      | CallExpr (recv, args) ->
         begin
           print recv; print_string "(\n";
           List.iter (fun arg -> print arg; print_string ",\n") args;
           print_string ")\n"
         end

      | Id (name, _) ->
         begin
           print_string "id{"; print_string (string_of_id_string name); print_string "}"
         end

      | ReturnStmt _ ->
         begin
           print_string "return"
         end
      | ImportStmt _ ->
         begin
           print_string "import"
         end
      | MemberFunctionDefStmt _ ->
         begin
           print_string "member function"
         end
      | ClassDefStmt _ ->
         begin
           print_string "class def"
         end
      | ExternClassDefStmt _ ->
         begin
           print_string "extern class def"
         end
      | MemberVariableDefStmt _ ->
         begin
           print_string "member variable def"
         end
      | TemplateStmt _ ->
         begin
           print_string "template"
         end
      | AttrWrapperStmt _ ->
         begin
           print_string "attr wrapper"
         end
      | SubscriptingExpr _ ->
         begin
           print_string "sub scripting"
         end
      | NewExpr _ ->
         begin
           print_string "new"
         end
      | DeleteExpr _ ->
         begin
           print_string "delete"
         end
      | StatementTraitsExpr _ ->
         begin
           print_string "stmt traits"
         end
      | InstantiatedId _ ->
         begin
           print_string "InstantiatedId"
         end
      | Int32Lit (v, _) ->
         begin
           Printf.printf "Int32Lit %d" v
         end
      | BoolLit _ ->
         begin
           print_string "BoolLit"
         end
      | StringLit _ ->
         begin
           print_string "StringLit"
         end
      | ArrayLit _ ->
         begin
           print_string "ArrayLit"
         end
      | Error ->
         begin
           print_string "Error"
         end
      | ParamsList _ ->
         begin
           print_string "ParamsList"
         end
      | TemplateParamsList _ ->
         begin
           print_string "TemplateParamsList"
         end
      | VarInit _ ->
         begin
           print_string "VarInit"
         end
      | PrevPassNode _ ->
         begin
           print_string "PrevPassNode"
         end
      | NotInstantiatedNode _ ->
         begin
           print_string "NotInstantiatedNode"
         end
      | GenericId _ ->
         begin
           print_string "GenericId"
         end
      | GenericCallExpr _ ->
         begin
           print_string "GenericCallExpr"
         end
      | GenericFuncDef _ ->
         begin
           print_string "GenericFuncDef"
         end
      | NestedExpr _ ->
         begin
           print_string "NestedExpr"
         end
      | _ -> print_string "unknown"
  end
