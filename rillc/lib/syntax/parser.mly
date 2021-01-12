(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

%start <Ast.t> program_entry, expr_entry

%{
  let make ~l kind =
    let span = Ast.Span.create_from_lex_loc ~path:"" ~lex_loc:l in
    Ast.{kind; span }
%}

%%

(**)
program_entry:
    module_def EOF { $1 }

module_def:
    body = top_levels
    {
        make (Ast.Module body) ~l:$loc
    }

top_levels:
    top_level* { make (Ast.Stmts $1) ~l:$loc }

top_level:
    top_level_statement { $1 }

let top_level_statement :=
    s=import_statement; { s }
  | s=def_type_alias_stmt; SEMICOLON; { s }
  | s=def_function_statement(parameter_decls_list); { s }
  | s=extern_decl_statement; SEMICOLON; { s }
  | s=def_struct_stmt; { s }
  | s=def_trait_stmt; { s }
  | s=def_impl_for_stmt; { s }

let import_statement :=
    KEYWORD_IMPORT; t=import_tree_root; SEMICOLON;
    {
      let (pkg, mods) = t in
      make (Ast.Import { pkg; mods }) ~l:$loc
    }

let import_tree_root :=
    root=single_id; mods=import_tree_node; { (root, mods) }

let import_tree_node :=
    COLONCOLON; id=single_id; child=import_tree_node; { id :: child }
  | COLONCOLON; id=import_tree_leaf; { [id] }

let import_tree_leaf ==
  | id=single_id; { id }
  | TIMES; { make Ast.IDWildcard ~l:$loc }

let def_type_alias_stmt :=
    KEYWORD_TYPE; name=single_id_as_str; ASSIGN; alias_ty=type_expr;
    { make (Ast.DefTypeAlias { name; alias_ty }) ~l:$loc }

let def_function_statement(params_list) ==
    KEYWORD_DEF;
    name=single_id_as_str;
    ty_params=type_parameter_decls_list;
    params=params_list;
    ret_ty=type_spec;
    body=expr_block;
    {
      make (Ast.DefFunc {
                name;
                ty_params;
                params;
                ret_ty;
                body;
           })
           ~l:$loc
    }

let parameter_decls_list ==
    LPAREN; xs=separated_list(COMMA, parameter_decl); RPAREN; { xs }

parameter_decl:
    v=decl_var(type_spec)
    {
      let (attr, name, ty_spec) = v in
      make (Ast.ParamDecl { attr; name; ty_spec; })
           ~l:$loc
    }

let parameter_decls_self_list ==
    LPAREN; x=self_decl; RPAREN; { [x] }
  | LPAREN;
    x=self_decl; COMMA;
    xs=separated_nonempty_list(COMMA, parameter_decl);
    RPAREN;
    { x :: xs }

self_decl:
    v=decl_var_self(type_spec)
    {
      let (attr, ty_spec_opt) = v in
      make (Ast.ParamSelfDecl { attr; ty_spec_opt; }) ~l:$loc
    }

let type_parameter_decls_list ==
    { [] }
    | NOT; LPAREN; xs=separated_list(COMMA, type_parameter_decl); RPAREN; { xs }

let type_parameter_decl ==
    name=single_id_as_str;
    { make (Ast.TyParamDecl { name; }) ~l:$loc }

let type_spec :=
    COLON; e=type_expr; { e }

let type_expr :=
    e=type_id_expr; { e }
  | LBRACKET; e=expr; RBRACKET; t=type_expr;
    { make (Ast.TypeExprArray { elem = t; len = e }) ~l:$loc }
  | TIMES; attr=decl_attr; t=type_expr;
    { make (Ast.TypeExprPointer { attr; elem = t; }) ~l:$loc }

let type_id_expr :=
  e=id_expr; { e }

extern_decl_statement:
    extern_function_decl_statement { $1 }
  | decl_extern_static_var_stmt { $1 }

extern_function_decl_statement:
    KEYWORD_EXTERN KEYWORD_DEF
    name = single_id_as_str
    params = parameter_decls_list
    ret_ty = type_spec
    ASSIGN
    symbol_name = lit_string
    {
      make (Ast.DeclExternFunc {
                name;
                ty_params = [];
                params;
                ret_ty;
                symbol_name;
           })
           ~l:$loc
    }

let decl_extern_static_var_stmt :=
    KEYWORD_EXTERN; KEYWORD_STATIC;
    attr=decl_attr; name=single_id_as_str; ty_spec=type_spec;
    {
      make (Ast.DeclExternStaticVar { attr; name; ty_spec }) ~l:$loc
    }

let def_struct_stmt :=
    KEYWORD_STRUCT;
    name=single_id_as_str;
    LBLOCK;
    RBLOCK;
    { make (Ast.DefStruct { name }) ~l:$loc }

let def_trait_stmt :=
    KEYWORD_TRAIT;
    name=single_id_as_str;
    LBLOCK;
    decls=trait_fields;
    RBLOCK;
    { make (Ast.DefTrait { name; decls }) ~l:$loc }

let trait_fields ==
    xs=trait_field*; { make (Ast.Stmts xs) ~l:$loc }

let trait_field :=
    s=decl_function_stmt(parameter_decls_self_list); SEMICOLON; { s }

let decl_function_stmt(params_list) ==
    KEYWORD_DEF;
    name=single_id_as_str;
    ty_params=type_parameter_decls_list;
    params=params_list;
    ret_ty = type_spec;
    {
      make (Ast.DeclFunc {
                name;
                ty_params;
                params;
                ret_ty;
           })
           ~l:$loc
    }

let def_impl_for_stmt :=
    KEYWORD_IMPL;
    trait=type_id_expr;
    KEYWORD_FOR;
    for_ty=type_expr;
    LBLOCK;
    decls=impl_fields;
    RBLOCK;
    { make (Ast.DefImplFor { trait; for_ty; decls }) ~l:$loc }

let impl_fields ==
    xs=impl_field*; { make (Ast.Stmts xs) ~l:$loc }

let impl_field :=
    s=def_function_statement(parameter_decls_list); { s }
  | s=def_function_statement(parameter_decls_self_list); { s }

let stmts :=
    { [] }
  | e=stmt_expr_apply(expr_without_block); { [e] }
  | s=stmt_expr(expr_without_block); SEMICOLON; ss=stmts; { s :: ss }
  | e=expr_with_block; ss=stmts; { e :: ss }
  | s=stmt_let; ss=stmts; { s :: ss }

let stmt_expr(expr) ==
    e=expr ; { make (Ast.StmtExpr e) ~l:$loc }

let stmt_expr_apply(expr) ==
    e=expr ; { make (Ast.StmtExprApply e) ~l:$loc }

stmt_let:
    KEYWORD_LET d = decl_var_expr SEMICOLON { make (Ast.StmtLet d) ~l:$loc }

decl_attr:
  { make Ast.DeclAttrImmutable ~l:$loc }
  | KEYWORD_MUTABLE { make Ast.DeclAttrMutable ~l:$loc }

let decl_var(type_anot) ==
    attr=decl_attr;
    name=single_id_as_str;
    ty_spec=type_anot;
    { (attr, name, ty_spec) }

let decl_var_self(type_anot) ==
    attr=decl_attr;
    name=KEYWORD_SELF;
    ty_spec=type_anot?;
    { (attr, ty_spec) }

decl_var_expr:
    v=decl_var(type_spec?)
    ASSIGN
    e = expr
    {
      let (attr, name, ty_spec) = v in
      make (Ast.VarDecl {
              attr = attr;
              name = name;
              ty_spec = ty_spec;
              expr = e
           })
           ~l:$loc
    }

stmt_return:
    KEYWORD_RETURN
    e = expr?
    SEMICOLON
    { make (Ast.StmtReturn e) ~l:$loc }

id_expr:
    single_id_with_self { $1 }

expr_entry:
    expr EOF { $1 }

expr:
    expr_with_block { $1 }
  | expr_without_block { $1 }

expr_with_block:
    expr_if { $1 }
  | expr_loop { $1 }
  | expr_block { $1 }

let expr_without_block :=
    e = expr_assign; { e }

let expr_block :=
  LBLOCK; ss=stmts; RBLOCK; { make (Ast.ExprBlock ss) ~l:$loc }

let expr_if :=
    KEYWORD_IF;
    cond = as_grouped_expr(expr_infix);
    then_n = expr_block;
    else_n_opt = expr_if_else_clause?;
    { make (Ast.ExprIf (cond, then_n, else_n_opt)) ~l:$loc }

let expr_if_else_clause ==
    KEYWORD_ELSE; e = expr_block; { e }
  | KEYWORD_ELSE; e = expr_if; { e }

let expr_loop ==
    e=expr_loop_infinity; { e }

let expr_loop_infinity :=
    KEYWORD_LOOP;
    e=expr_block;
    { make (Ast.ExprLoop e) ~l:$loc }

let as_grouped_expr(expr) ==
    e=expr; { make (Ast.ExprGrouping e) ~l:$loc }

(* right-associativity *)
let expr_assign :=
    lhs=as_grouped_expr(expr_infix); ASSIGN; rhs=expr_assign;
    { make (Ast.ExprAssign {lhs; rhs}) ~l:$loc }
  | e=as_grouped_expr(expr_infix); { e }

expr_infix:
    lhs = expr_infix op = infix_id rhs = expr_prefix
    { make (Ast.ExprBinaryOp {op; lhs; rhs}) ~l:$loc }
  | expr_prefix { $1 }

let expr_prefix :=
    e=expr_postfix; { e }
  | e=expr_ref; { e }
  | e=expr_deref; { e }

let expr_ref ==
    BITWISE_AND; attr=decl_attr; e=expr_prefix;
    { make (Ast.ExprRef (attr, e)) ~l:$loc }

let expr_deref ==
    TIMES; e=expr_prefix;
    { make (Ast.ExprDeref e) ~l:$loc }

expr_postfix:
    expr_primary { $1 }
  | expr_call { $1 }
  | expr_index { $1 }
  | expr_break { $1 }
  | expr_as { $1 }

let expr_call ==
    r=expr_postfix; LPAREN; args=argument_list; RPAREN;
    { make (Ast.ExprCall (r, args)) ~l:$loc }

let expr_index ==
    r=expr_postfix; LBRACKET; e=expr; RBRACKET;
    { make (Ast.ExprIndex (r, e)) ~l:$loc }

let expr_break ==
    KEYWORD_BREAK;
    { make (Ast.ExprBreak) ~l:$loc }

let expr_as ==
    expr=expr_postfix; KEYWORD_AS; ty_expr=type_expr;
    { make (Ast.ExprAs { expr; ty_expr }) ~l:$loc }

let expr_primary :=
    e=expr_struct; { e }
  | e=value; { e }
  | LPAREN; e=expr; RPAREN; { e }

let expr_struct ==
    path=id_expr; LBLOCK; RBLOCK;
    { make (Ast.ExprStruct { path }) ~l:$loc }

let value :=
    v=id_path; { v }
  | v=lit_bool; { v }
  | v=lit_integer; { v }
  | v=lit_string; { v }
  | v=lit_array; { v }

argument_list:
    separated_list(COMMA, expr) { $1 }

let id_path :=
    root=single_id_with_self; elems=id_path_node;
    { make (Ast.Path { root; elems }) ~l:$loc }

let id_path_node :=
    { [] }
  | COLONCOLON; id=single_id_with_self; child=id_path_node; { id :: child }

single_id:
    single_id_as_str { make (Ast.ID $1) ~l:$loc }

single_id_with_self:
    single_id { $1 }
  | KEYWORD_SELF { make (Ast.ID $1) ~l:$loc }

single_id_as_str:
  | ID { $1 }
  | SINGLEQUOTE infix_id_as_str SINGLEQUOTE { $2 }

infix_id:
    infix_id_as_str { make (Ast.ID $1) ~l:$loc }

infix_id_as_str:
    PLUS        { $1 }
  | MINUS       { $1 }
  | TIMES       { $1 }
  | DIV         { $1 }
  | MOD         { $1 }
  | GT          { $1 }
  | GTE         { $1 }
  | LT          { $1 }
  | LTE         { $1 }
  | LSHIFT      { $1 }
  | ARSHIFT     { $1 }
  | LRSHIFT     { $1 }
  | EQUALS      { $1 }
  | NOT_EQUALS  { $1 }
  | LOGICAL_OR  { $1 }
  | LOGICAL_AND { $1 }
  | BITWISE_AND { $1 }
  | BITWISE_OR  { $1 }
  | BITWISE_XOR { $1 }

(**)
lit_bool:
    LIT_TRUE { make (Ast.LitBool true) ~l:$loc }
  | LIT_FALSE { make (Ast.LitBool false) ~l:$loc }

lit_integer:
    INT
    {
      let (v, bits, is_signed) = $1 in
      make (Ast.LitInt (v, bits, is_signed)) ~l:$loc
    }

lit_string:
    STRING { make (Ast.LitString $1) ~l:$loc }

let lit_array :=
    LBRACKET; es=lit_array_elems; RBRACKET;
    { make (Ast.LitArrayElems es) ~l:$loc }

let lit_array_elems ==
    es=separated_list(COMMA, expr); { es }
