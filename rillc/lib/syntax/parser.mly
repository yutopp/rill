(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

%start <Ast.t> program_entry

%nonassoc IFX
%nonassoc KEYWORD_ELSE

%{
  let make ~l kind =
    Ast.{kind; span = Span.create_from_lex_loc ~path:"" ~lex_loc:l}
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
    ret_ty = type_spec?
    body = function_def_body
    {
      make (Ast.FunctionDefStmt {
              name = name;
              ret_ty = ret_ty;
              params = params;
              body = body;
           })
           ~l:$loc
    }

function_def_body:
    expr_compound { $1 }

parameter_decls_list:
    separated_list(COMMA, parameter_decl) { $1 }

parameter_decl:
    name = single_id_as_str
    ty_spec = type_spec
    {
      make (Ast.ParamDecl {
              name = name;
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
    ret_ty = type_spec?
    ASSIGN
    symbol_name = lit_string
    {
      make (Ast.ExternFunctionDeclStmt {
              name = name;
              ret_ty = ret_ty;
              params = params;
              symbol_name = symbol_name;
           })
           ~l:$loc
    }

stmt:
    stmt_expr { $1 }
  | stmt_return { $1 }
  | stmt_condition { $1 }

stmt_expr:
    expr_assign SEMICOLON { make (Ast.StmtExpr $1) ~l:$loc }

stmt_return:
    KEYWORD_RETURN
    e = expr_assign?
    SEMICOLON
    { make (Ast.StmtReturn e) ~l:$loc }

stmt_condition:
    expr_if { $1 }

id_expr:
    expr_primary { $1 }

expr:
    expr_if { $1 }

expr_if:
    expr_compound { $1 }
  | KEYWORD_IF
    cond = expr_assign
    then_n = expr %prec IFX
    { make (Ast.ExprIf (cond, then_n, None)) ~l:$loc }
  | KEYWORD_IF
    cond = expr_assign
    then_n = expr
    KEYWORD_ELSE
    else_n = expr
    { make (Ast.ExprIf (cond, then_n, Some else_n)) ~l:$loc }

expr_compound:
    expr_assign { $1 }
  | LBLOCK stmt* RBLOCK { make (Ast.ExprCompound $2) ~l:$loc }

expr_assign:
    expr_postfix { $1 }

expr_postfix:
    expr_primary { $1 }
  | expr_postfix LPAREN argument_list RPAREN
    { make (Ast.ExprCall ($1, $3)) ~l:$loc }

argument_list:
    separated_list(COMMA, expr) { $1 }

expr_primary:
    value { $1 }
  | LPAREN expr RPAREN { $2 }

value:
    single_id { $1 }
  | lit_bool { $1 }
  | lit_integer { $1 }
  | lit_string { $1 }

single_id:
    single_id_as_str { make (Ast.ID $1) ~l:$loc }

single_id_as_str:
    ID { $1 }

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
