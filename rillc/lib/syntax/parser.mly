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

top_level_statement:
    function_def_statement { $1 }
  | extern_decl_statement SEMICOLON { $1 }

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
    name = single_id_as_str
    ty_spec = type_spec
    {
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
  | s=stmt_expr(expr_without_block); { [s] }
  | s=stmt_expr(expr_without_block); SEMICOLON; ss=stmts; { s :: ss }
  | s=stmt_expr(expr_with_block); ss=stmts; { s :: ss }
  | s=stmt_let; ss=stmts; { s :: ss }

let stmt_expr(expr) ==
    e=expr ; { make (Ast.StmtExpr e) ~l:$loc }

stmt_let:
    KEYWORD_LET d = stmt_let_decl_val SEMICOLON { make (Ast.StmtLet d) ~l:$loc }

decl_attr:
  { make Ast.DeclAttrImmutable ~l:$loc }
  | KEYWORD_MUTABLE { make Ast.DeclAttrMutable ~l:$loc }

stmt_let_decl_val:
    attr = decl_attr
    name = single_id_as_str
    ty_spec = type_spec?
    ASSIGN
    e = expr
    {
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
  | expr_block { $1 }

let expr_without_block :=
    e = expr_infix_group; { e }

let expr_block :=
  LBLOCK; ss=stmts; RBLOCK; { make (Ast.ExprBlock ss) ~l:$loc }

expr_if:
  | KEYWORD_IF
    cond = expr_infix_group
    then_n = expr_block
    { make (Ast.ExprIf (cond, then_n, None)) ~l:$loc }
  | KEYWORD_IF
    cond = expr_infix_group
    then_n = expr_block
    KEYWORD_ELSE
    else_n = expr_block
    { make (Ast.ExprIf (cond, then_n, Some else_n)) ~l:$loc }

expr_infix_group:
    expr_infix { make (Ast.ExprGrouping $1) ~l:$loc }

expr_infix:
    lhs = expr_infix op = infix_id rhs = expr_postfix
    { make (Ast.ExprBinaryOp {op; lhs; rhs}) ~l:$loc }
  | expr_postfix { $1 }

expr_postfix:
    expr_primary { $1 }
  | expr_postfix LPAREN argument_list RPAREN
    { make (Ast.ExprCall ($1, $3)) ~l:$loc }

expr_primary:
    value { $1 }
  | LPAREN expr RPAREN { $2 }

value:
    single_id { $1 }
  | lit_bool { $1 }
  | lit_integer { $1 }
  | lit_string { $1 }

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
  | ASSIGN      { $1 }

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
