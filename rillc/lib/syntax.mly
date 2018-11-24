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
  let make ~b ~e kind =
    Ast.{kind; span = Span.create ~b:b ~e:e}
%}

%%

(**)
program_entry:
    module_def EOF { $1}

module_def:
    body = top_levels
    {
        make (Ast.Module body) ~b:$startpos ~e:$endpos
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
           ~b:$startpos
           ~e:$endpos
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
           ~b:$startpos ~e:$endpos
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
           ~b:$startpos
           ~e:$endpos
    }

stmt:
    stmt_expr { $1 }
  | stmt_return { $1 }
  | stmt_condition { $1 }

stmt_expr:
    expr_assign SEMICOLON { make (Ast.StmtExpr $1) ~b:$startpos ~e:$endpos }

stmt_return:
    KEYWORD_RETURN
    e = expr_assign?
    SEMICOLON
    { make (Ast.StmtReturn e) ~b:$startpos ~e:$endpos }

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
    { make (Ast.ExprIf (cond, then_n, None)) ~b:$startpos ~e:$endpos }
  | KEYWORD_IF
    cond = expr_assign
    then_n = expr
    KEYWORD_ELSE
    else_n = expr
    { make (Ast.ExprIf (cond, then_n, Some else_n)) ~b:$startpos ~e:$endpos }

expr_compound:
    expr_assign { $1 }
  | LBLOCK stmt* RBLOCK { make (Ast.ExprCompound $2) ~b:$startpos ~e:$endpos }

expr_assign:
    expr_postfix { $1 }

expr_postfix:
    expr_primary { $1 }
  | expr_postfix LPAREN argument_list RPAREN
    { make (Ast.ExprCall ($1, $3)) ~b:$startpos ~e:$endpos }

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
    single_id_as_str { make (Ast.ID $1) ~b:$startpos ~e:$endpos }

single_id_as_str:
    ID { $1 }

(**)
lit_bool:
    LIT_TRUE { make (Ast.LitBool true) ~b:$startpos ~e:$endpos }
  | LIT_FALSE { make (Ast.LitBool false) ~b:$startpos ~e:$endpos }

lit_integer:
    INT
    {
      let (v, bits, is_signed) = $1 in
      make (Ast.LitInt (v, bits, is_signed)) ~b:$startpos ~e:$endpos
    }

lit_string:
    STRING { make (Ast.LitString $1) ~b:$startpos ~e:$endpos }
