(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Loc = struct
  type info_t = {
    pos_fname           : string;
    source_code         : bytes;
    pos_begin_cnum      : int;
    pos_begin_lnum      : int;
    pos_begin_bol       : int;
    pos_end_cnum        : int;
    pos_end_lnum        : int;
    pos_end_bol         : int;
  }

  type t = info_t option
  let dummy = None

  let to_string opt_loc =
    match opt_loc with
    | Some loc ->
       let bline = loc.pos_begin_lnum in
       let bcol = loc.pos_begin_bol in
       let eline = loc.pos_end_lnum in
       let ecol = loc.pos_end_bol in
       let pos_s = if bline = eline then
                     Printf.sprintf "Line %d, charactor %d-%d"
                                    bline bcol ecol
                   else
                     Printf.sprintf "Line %d, charactor %d to Line %d, %d"
                                    bline bcol eline ecol
       in
       Printf.sprintf "%s in %s" pos_s loc.pos_fname
    | None -> "Unknown location"
end


type id_string =
    Pure of string
  | UnaryPreOp of string
  | UnaryPostOp of string
  | BinaryOp of string

let string_of_id_string id_s =
  match id_s with
  | Pure s -> s
  | UnaryPreOp s -> "op_unary_pre_" ^ s
  | UnaryPostOp s -> "op_unary_post_" ^ s
  | BinaryOp s -> "op_binary_" ^ s


module type NodeContextType =
  sig
    type 'a current_ctx_t
    type 'a term_ctx_t
    type 'a prev_ctx_t
  end

module CachedNodeCounter = Generic_counter.Counter(Int64)

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
      (* name, params, return_type?, body, attribute?, _ *)
      | MemberFunctionDefStmt of id_string * ast * ast option * ast * attr_tbl_t option * ctx_t
      (* name, params, return_type, function name(TODO: change to AST), attribute?, _ *)
      | ExternFunctionDefStmt of id_string * ast * Meta_level.t * ast * string * attr_tbl_t option * ctx_t
      (* name, lifetime, body, attribute?, _ *)
      | ClassDefStmt of id_string * ast * attr_tbl_t option * ctx_t
      | ExternClassDefStmt of id_string * lifetime_spec * string * attr_tbl_t option * ctx_t
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
      | BinaryOpExpr of ast * ast * ast * term_ctx_t    (* lhs * op * rhs *)
      | UnaryOpExpr of ast * ast * term_ctx_t           (* op * rhs *)

      | ElementSelectionExpr of ast * ast * term_ctx_t
      | SubscriptingExpr of ast * ast option * term_ctx_t
      | CallExpr of ast * ast list * term_ctx_t
      | ScopeExpr of ast
      | IfExpr of ast * ast * ast option * term_ctx_t
      | ForExpr of ast option * ast option * ast option * ast
      | NewExpr of ast
      | DeleteExpr of ast
      | StatementTraitsExpr of string * ast

      (* used for calling destructors *)
      | FinalyzeExpr of ast option * ast list
      (* set cache id for only needed ones. will be used for memo needed by destructor *)
      | SetCacheExpr of CachedNodeCounter.t * ast
      | GetCacheExpr of CachedNodeCounter.t

      (*
       * values
       *)
      | Id of id_string * term_ctx_t
      | InstantiatedId of id_string * ast list * term_ctx_t
      | IntLit of int * int * bool * term_ctx_t (* value * bits * signed *)
      | StringLit of string * term_ctx_t
      | BoolLit of bool * term_ctx_t
      | ArrayLit of ast list * bool * term_ctx_t

      (* error *)
      | Error

      (* special *)
      | ParamsList of param_init_t list
      | TemplateParamsList of template_param_init_t list
      | VarInit of var_init_t
      | PrevPassNode of pctx_t
      | NotInstantiatedNode of pctx_t * attr_tbl_t option

      | CtxNode of term_ctx_t
      | TypeRVConv of Type_attr.ref_val_t * ast list * term_ctx_t
      | TypeQualConv of Type_attr.mut_t * ast list * term_ctx_t
      | MetaLevelConv of Meta_level.t * ast list * term_ctx_t

      (* *)
      | GenericId of id_string * ctx_t
      (* object construction, args, ctx *)
      | GenericCallExpr of storage_t * ast list * ctx_t * ctx_t
      (* body, ctx *)
      | GenericFuncDef of ast option * ctx_t
      | NestedExpr of ast * term_aux_t * term_ctx_t * ctx_t
      | StorageWrapperExpr of storage_t ref * ast

     and term_aux_t = (term_ctx_t * Value_category.t * Lifetime.t * Meta_level.t * Loc.t)

     (* attr * id? * value *)
     and param_init_t = Type_attr.attr_t * string option * value_init_t
     (* id * value? *)
     and template_param_init_t = string * value_init_t option
     (* attr * id * value *)
     and var_init_t = var_aux_t * string * value_init_t
     and var_aux_t = Type_attr.attr_t

     and lifetime_ids = string list
     and lifetime_spec = lifetime_ids

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
       | StoAgg of term_ctx_t
       | StoImm
       (* type of element * index *)
       | StoArrayElem of term_ctx_t * int
       (* type of element * env of this * index *)
       | StoArrayElemFromThis of term_ctx_t * ctx_t * int
       (* *)
       | StoMemberVar of term_ctx_t * ctx_t * ctx_t

    type t = ast

    let debug_print_storage sto =
      match sto with
      | StoStack _ -> Debug.printf "StoStack\n"
      | StoHeap -> Debug.printf "StoHeap\n"
      | StoGc -> Debug.printf "StoGc\n"
      | StoAgg _ -> Debug.printf "StoAgg\n"
      | StoImm -> Debug.printf "StoImm\n"
      | StoArrayElem _ -> Debug.printf "StoArrayElem\n"
      | StoArrayElemFromThis _ -> Debug.printf "StoArrayElemFromThis\n"
      | StoMemberVar _ -> Debug.printf "StoMemberVar\n"

    let rec print ast =
      match ast with
      | Module (a, _, _, _, ctx) ->
         begin
           Debug.printf "module\n";
           print a;
         end

      | StatementList asts ->
         begin
           Debug.printf "StatementList\n";
           asts |> List.iter (fun a -> print a; Debug.printf "\n")
         end

      | ExprStmt _ ->
         Debug.printf "ExprStmt\n"

      | FunctionDefStmt (id, _, _, _, statements, _, ctx) ->
         begin
           Debug.printf "function def : ";
           Debug.printf "%s\n" (string_of_id_string id);
           print statements;
         end

      | ExternFunctionDefStmt _ ->
         Debug.printf "ExternFunctionDefStmt\n"

      | VariableDefStmt _ ->
         Debug.printf "VariableDefStmt\n"

      | EmptyStmt ->
         begin
           Debug.printf "EMPTY\n";
         end

      | BinaryOpExpr (lhs, op, rhs, _) ->
         begin
           Debug.printf "binary\n";
           print lhs; print op; print rhs
         end

      | UnaryOpExpr (op, expr, _) ->
         begin
           Debug.printf "unary\n";
           print op; print expr
         end

      | ElementSelectionExpr (recv, sel, _) ->
         begin
           Debug.printf "element selector\n";
           print recv; Debug.printf "."; print sel
         end

      | CallExpr (recv, args, _) ->
         begin
           Debug.printf "Call\n";
           print recv; Debug.printf "(\n";
           List.iter (fun arg -> print arg; Debug.printf ",\n") args;
           Debug.printf ")\n"
         end

      | Id (name, _) ->
         begin
           Debug.printf "id{";
           Debug.printf "%s" (string_of_id_string name);
           Debug.printf "}"
         end

      | ReturnStmt _ ->
         begin
           Debug.printf "return\n"
         end
      | ImportStmt _ ->
         begin
           Debug.printf "import\n"
         end
      | MemberFunctionDefStmt _ ->
         begin
           Debug.printf "member function\n"
         end
      | ClassDefStmt _ ->
         begin
           Debug.printf "class def\n"
         end
      | ExternClassDefStmt _ ->
         begin
           Debug.printf "extern class def\n"
         end
      | MemberVariableDefStmt _ ->
         begin
           Debug.printf "member variable def\n"
         end
      | TemplateStmt _ ->
         begin
           Debug.printf "template\n"
         end
      | AttrWrapperStmt _ ->
         begin
           Debug.printf "attr wrapper\n"
         end
      | SubscriptingExpr _ ->
         begin
           Debug.printf "sub scripting\n"
         end
      | NewExpr _ ->
         begin
           Debug.printf "new\n"
         end
      | DeleteExpr _ ->
         begin
           Debug.printf "delete\n"
         end
      | StatementTraitsExpr _ ->
         begin
           Debug.printf "stmt traits\n"
         end
      | InstantiatedId _ ->
         begin
           Debug.printf "InstantiatedId\n"
         end
      | IntLit (v, bits, signed, _) ->
         begin
           Printf.printf "Int32Lit %d\n" v
         end
      | BoolLit _ ->
         begin
           Debug.printf "BoolLit\n"
         end
      | StringLit _ ->
         begin
           Debug.printf "StringLit\n"
         end
      | ArrayLit _ ->
         begin
           Debug.printf "ArrayLit\n"
         end
      | Error ->
         begin
           Debug.printf "Error\n"
         end
      | ParamsList _ ->
         begin
           Debug.printf "ParamsList\n"
         end
      | TemplateParamsList _ ->
         begin
           Debug.printf "TemplateParamsList\n"
         end
      | VarInit _ ->
         begin
           Debug.printf "VarInit\n"
         end
      | PrevPassNode _ ->
         begin
           Debug.printf "PrevPassNode\n"
         end
      | NotInstantiatedNode _ ->
         begin
           Debug.printf "NotInstantiatedNode\n"
         end
      | GenericId _ ->
         begin
           Debug.printf "GenericId\n"
         end
      | GenericCallExpr _ ->
         begin
           Debug.printf "GenericCallExpr\n"
         end
      | GenericFuncDef _ ->
         begin
           Debug.printf "GenericFuncDef\n"
         end
      | NestedExpr _ ->
         begin
           Debug.printf "NestedExpr\n"
         end
      | ScopeExpr _ ->
         begin
           Debug.printf "ScopeExpr\n"
         end
      | IfExpr _ ->
         begin
           Debug.printf "IfExpr\n"
         end
      | ForExpr _ ->
         begin
           Debug.printf "ForExpr\n"
         end
      | FinalyzeExpr _ ->
         begin
           Debug.printf "FinalyzeExpr\n"
         end
      | SetCacheExpr _ ->
         begin
           Debug.printf "SetCacheExpr\n"
         end
      | GetCacheExpr _ ->
         begin
           Debug.printf "GetCacheExpr\n"
         end
      | CtxNode _ ->
         begin
           Debug.printf "CtxNode\n"
         end
      | TypeQualConv _ ->
         begin
           Debug.printf "TypeQualConv\n"
         end
      | TypeRVConv _ ->
         begin
           Debug.printf "TypeRVConv\n"
         end
      | MetaLevelConv _ ->
         begin
           Debug.printf "MetaLevelConv\n"
         end
      | StorageWrapperExpr _ ->
         begin
           Debug.printf "StorageWrapperExpr\n"
         end
  end
