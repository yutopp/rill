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
    top_level* { $1 }

top_level:
    top_level_statement { $1 }

let top_level_statement :=
    s=import_statement; { s }
  | s=function_def_statement; { s }
  | s=extern_decl_statement; SEMICOLON; { s }

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

function_def_statement:
    KEYWORD_DEF
    name = single_id_as_str
    LPAREN params = parameter_decls_list RPAREN
    ret_ty = type_spec
    body = expr_block
    {
      make (Ast.DefFunc {
              name = name;
              ret_ty = ret_ty;
              params = params;
              body = body;
           })
           ~l:$loc
    }

parameter_decls_list:
    separated_list(COMMA, parameter_decl) { $1 }

parameter_decl:
    v=decl_var(type_spec)
    {
      let (attr, name, ty_spec) = v in
      make (Ast.ParamDecl {
              name = name;
              ty_spec = ty_spec;
           })
           ~l:$loc
    }

type_spec:
    COLON id_expr { $2 }

extern_decl_statement:
    extern_function_decl_statement { $1 }

extern_function_decl_statement:
    KEYWORD_EXTERN KEYWORD_DEF
    name = single_id_as_str
    LPAREN params = parameter_decls_list RPAREN
    ret_ty = type_spec
    ASSIGN
    symbol_name = lit_string
    {
      make (Ast.DeclExternFunc {
              name = name;
              ret_ty = ret_ty;
              params = params;
              symbol_name = symbol_name;
           })
           ~l:$loc
    }

let stmts :=
    { [] }
  | e=expr_without_block; { [e] }
  | s=stmt_expr(expr_without_block); SEMICOLON; ss=stmts; { s :: ss }
  | e=expr_with_block; ss=stmts; { e :: ss }
  | s=stmt_let; ss=stmts; { s :: ss }

let stmt_expr(expr) ==
    e=expr ; { make (Ast.StmtExpr e) ~l:$loc }

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
    single_id { $1 }

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
    lhs = expr_infix op = infix_id rhs = expr_postfix
    { make (Ast.ExprBinaryOp {op; lhs; rhs}) ~l:$loc }
  | expr_postfix { $1 }

expr_postfix:
    expr_primary { $1 }
  | expr_call { $1 }
  | expr_index { $1 }
  | expr_break { $1 }

let expr_call ==
    r=expr_postfix; LPAREN; args=argument_list; RPAREN;
    { make (Ast.ExprCall (r, args)) ~l:$loc }

let expr_index ==
    r=expr_postfix; LBRACKET; e=expr; RBRACKET;
    { make (Ast.ExprIndex (r, e)) ~l:$loc }

let expr_break ==
    KEYWORD_BREAK;
    { make (Ast.ExprBreak) ~l:$loc }

expr_primary:
    value { $1 }
  | LPAREN expr RPAREN { $2 }

value:
    single_id { $1 }
  | lit_bool { $1 }
  | lit_integer { $1 }
  | lit_string { $1 }
  | lit_array { $1 }

argument_list:
    separated_list(COMMA, expr) { $1 }

single_id:
    single_id_as_str { make (Ast.ID $1) ~l:$loc }

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
