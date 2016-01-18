%token  <int>           INT
%token  <string>        ID
%token                  PLUS MINUS TIMES DIV
                        ASSIGN
%token                  LPAREN RPAREN
                        LBLOCK RBLOCK
%token                  COMMA
                        COLON
                        SEMICOLON
                        FAT_ARROW
%token                  CR EOF
%token                  DECL_DEF
                        KEYWORD_VAL
                        KEYWORD_REF

%start <Ast.t>          program_entry

%nonassoc error

%%

(**)
program_entry:
                prog_module EOF { $1 }

prog_module:
                top_level_statements { Ast.Module ($1, ()) }

top_level_statements:
                top_level_statement* { Ast.StatementList $1 }


(* statements *)

top_level_statement:
                empty_statement { $1 }
        |       function_decl_statement { $1 }


empty_statement:
                SEMICOLON { Ast.EmptyStmt }

function_decl_statement:
                DECL_DEF
                id_rel
                parameter_variables_decl_list
                function_decl_body_block
                {
                    Ast.FunctionDefStmt ($2, $3, $4, ())
                }

function_decl_body_block:
                function_body_block             { $1 }
        |       function_lambda_block SEMICOLON { $1 }

function_body_block:
                LBLOCK
                program_body_statements
                RBLOCK
                { $2 }

function_lambda_block:
                FAT_ARROW
                expression
                { Ast.EmptyStmt (* tmp *) }

(**)
parameter_variables_decl_list:
                LPAREN
                separated_list(COMMA, parameter_variable_declaration)
                RPAREN { Ast.ParamsList $2 }

parameter_variable_declaration:
                parameter_variable_initializer_unit { $1 }

parameter_variable_initializer_unit:
                id_rel? value_initializer_unit { ($1, $2) }

(*
    :int = 5
    = 5
    :int
*)
value_initializer_unit:
                value_initializer_unit_only_value { (None, Some $1) }
        |       type_specifier option(value_initializer_unit_only_value) { (Some $1, $2) }


value_initializer_unit_only_value:
                ASSIGN expression { $2 }

(**)
program_body_statement:
                (*  block_statement
                * | variable_declaration_statement
                * | control_flow_statement
                * | return_statement
                * | empty_statement
                * | expression_statement
                *)
                variable_declaration_statement { $1 }

program_body_statements:
                program_body_statement* { Ast.StatementList ($1) }


(**)
type_specifier:
                COLON id_expression { $2 }


(**)
variable_decl_introducer:
                KEYWORD_VAL { Type.Attr.Val }
        |       KEYWORD_REF { Type.Attr.Ref }

variable_declaration_statement:
                variable_declararion SEMICOLON {
                    let (rv, v) = $1 in
                    Ast.VariableDefStmt (rv, Ast.VarInit v)
                }

variable_declararion:
                variable_decl_introducer variable_initializer_unit
                { ($1, $2) }

variable_initializer_unit:
                id_rel value_initializer_unit { ($1, $2) }


(**)
id_expression:
                id_rel { Ast.Id ($1) }


expression:
                INT { Ast.Int32Lit ($1) }


(**)
id_rel:         ID { $1 }
