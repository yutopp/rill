(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module type NodeContextType =
  sig
    type 'a current_ctx_t
    type 'a term_ctx_t
    type 'a prev_ctx_t
    type 'a term_aux_t
    type 'a attr_value_t
  end

module CachedNodeCounter = Generic_counter.Counter(Int64)

type qual_t =
    QualMutable
  | QualConst
  | QualImmutable

module Make (Ctx : NodeContextType) =
  struct
    type kind_t =
        Module of t * string list * string * string * ctx_t

      (*
       * statements
       *)
      | StatementList of t list
      | ExprStmt of t
      | VoidExprStmt of t
      | ReturnStmt of t option
      | ImportStmt of string list * string * bool * ctx_t
      (* name, lifetimes, params, meta_level, return_type?, instance_cond, body, attribute?, _ *)
      | FunctionDefStmt of Id_string.t * lifetime_def_specs * t * Meta_level.t * t option * t option * t * attr_tbl_t option * ctx_t
      (* name, lifetimes, params, quals, return_type?, body, attribute?, _ *)
      | MemberFunctionDefStmt of Id_string.t * lifetime_def_specs * t * qual_t list * t option * t * attr_tbl_t option * ctx_t
      (* name, lifetimes, params, meta_level, return_type, instance_cond, function name(TODO: change to AST), attribute?, _ *)
      | ExternFunctionDefStmt of Id_string.t * lifetime_def_specs * t * Meta_level.t * t * t option * string * attr_tbl_t option * ctx_t
      (* name, lifetime, body, attribute?, _ *)
      | ClassDefStmt of Id_string.t * lifetime_def_specs * t * attr_tbl_t option * ctx_t
      (* name, lifetimes, params, class name(TODO: change to AST), body?, attribute?, _ *)
      | ExternClassDefStmt of Id_string.t * lifetime_def_specs * string * t option * attr_tbl_t option * ctx_t
      (* VarInit, _ *)
      | VariableDefStmt of Meta_level.t * t * ctx_t
      | MemberVariableDefStmt of t * ctx_t

      (* name, template params, inner node *)
      | TemplateStmt of Id_string.t * t * t
      | EmptyStmt
      | AttrWrapperStmt of attr_tbl_t * t

      (*
       * expressions
       *)
      | BinaryOpExpr of t * t * t * term_ctx_t    (* lhs * op * rhs *)
      | UnaryOpExpr of t * t * term_ctx_t           (* op * rhs *)

      | ElementSelectionExpr of t * t * term_ctx_t
      | SubscriptingExpr of t * t option * term_ctx_t
      | CallExpr of t * t list * term_ctx_t
      | ScopeExpr of t
      | IfExpr of t * t * t option * term_ctx_t
      | ForExpr of t option * t option * t option * t

      | StatementTraitsExpr of string * t

      (* used for calling destructors *)
      | FinalyzeExpr of t option * t list

      (**)
      (*| TerminalExpr of ast*)

      (* set cache id for only needed ones. will be used for memo needed by destructor *)
      | SetCacheExpr of CachedNodeCounter.t * t
      | GetCacheExpr of CachedNodeCounter.t

      (*
       * values
       *)
      | Id of Id_string.t * lifetime_specs * term_ctx_t
      | InstantiatedId of Id_string.t * t list * lifetime_specs * term_ctx_t

      | IntLit of int * int * bool * term_ctx_t (* value * bits * signed *)
      | StringLit of string * term_ctx_t
      | BoolLit of bool * term_ctx_t
      | ArrayLit of t list * bool * term_ctx_t

      (* error *)
      | ErrorTerm
      | Undef of term_ctx_t

      (* special *)
      | ParamsList of param_init_t list
      | TemplateParamsList of template_param_init_t list
      | VarInit of var_init_t
      | PrevPassNode of pctx_t
      | NotInstantiatedNode of pctx_t * attr_tbl_t option

      | CtxNode of term_ctx_t
      | TypeRVConv of Type_attr.ref_val_t * t list * term_ctx_t
      | TypeQualConv of Type_attr.mut_t * t list * term_ctx_t
      | MetaLevelConv of Meta_level.t * t list * term_ctx_t

      (* *)
      | GenericId of Id_string.t * Lifetime.t list * ctx_t
      (* object construction, args, caller env, ctx *)
      | GenericCallExpr of storage_t * t list * ctx_t * ctx_t
      (* body, ctx *)
      | GenericFuncDef of t option * ctx_t
      | NestedExpr of t * term_aux_t * term_ctx_t * ctx_t
      | StorageWrapperExpr of storage_t ref * t

     (* attr * id? * value *)
     and param_init_t = Type_attr.attr_t * Id_string.t option * value_init_t
     (* id * value? *)
     and template_param_init_t = Id_string.t * value_init_t option
     (* attr * id * value *)
     and var_init_t = var_aux_t * Id_string.t * value_init_t
     and var_aux_t = Type_attr.attr_t

     and lifetime_specs = Id_string.t list
     and lifetime_def_specs = Lifetime.sort list

     (* type * default value *)
     and value_init_t = t option * t option

     and attr_tbl_t = (string, attr_value_t option) Hashtbl.t

     and ctx_t = t Ctx.current_ctx_t
     and term_ctx_t = t Ctx.term_ctx_t
     and pctx_t = t Ctx.prev_ctx_t
     and term_aux_t = t Ctx.term_aux_t
     and attr_value_t = t Ctx.attr_value_t

     and storage_t =
       | StoStack of term_ctx_t
       | StoImm
       | StoHeap
       | StoGc
       (* bind storage *)
       | StoAgg of term_ctx_t
       (* type of element * index *)
       | StoArrayElem of term_ctx_t * int
       (* type of element * env of this * index *)
       | StoArrayElemFromThis of term_ctx_t * ctx_t * int
       (* *)
       | StoMemberVar of term_ctx_t * ctx_t * ctx_t

     and t = {
       kind: kind_t;
       loc: Loc.t
     }

    let kind_of node =
      let {kind} = node in
      kind

    let loc_of node =
      let {loc} = node in
      loc

    let map f node =
      let {kind; loc} = node in
      {kind = f kind; loc}

    let string_of_stirage sto =
      match sto with
      | StoStack _ -> "StoStack"
      | StoHeap -> "StoHeap"
      | StoGc -> "StoGc"
      | StoAgg _ -> "StoAgg"
      | StoImm -> "StoImm"
      | StoArrayElem _ -> "StoArrayElem"
      | StoArrayElemFromThis _ -> "StoArrayElemFromThis"
      | StoMemberVar _ -> "StoMemberVar"

    let debug_print_storage sto =
      Debug.printf "%s\n" (string_of_stirage sto)

    let rec print node =
      match kind_of node with
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

      | VoidExprStmt _ ->
         Debug.printf "VoidExprStmt\n"

      | FunctionDefStmt (id, _, _, _, _, _, statements, _, ctx) ->
         begin
           Debug.printf "function def : ";
           Debug.printf "%s\n" (Id_string.to_string id);
           print statements;
         end

      | ExternFunctionDefStmt _ ->
         Debug.printf "ExternFunctionDefStmt\n"

      | VariableDefStmt _ ->
         Debug.printf "VariableDefStmt\n"

      | EmptyStmt ->
         begin
           Debug.printf "EmptyStmt\n";
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

      | Id (name, _, _) ->
         begin
           Debug.printf "id{";
           Debug.printf "%s" (Id_string.to_string name);
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
           Debug.printf "Int32Lit %d\n" v
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
      | ErrorTerm ->
         begin
           Debug.printf "Error\n"
         end
      | Undef _ ->
         Debug.printf "Undef\n"

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
