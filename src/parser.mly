%token  <int>           INT
%token  <string>        ID
%token                  PLUS MINUS TIMES DIV
                        ASSIGN
%token                  LPAREN RPAREN
                        LBLOCK RBLOCK
%token                  COMMA
                        SEMICOLON
                        FAT_ARROW
%token                  CR EOF
%token                  DECL_DEF

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
                { Ast.EmptyStmt (* tmp *) }

parameter_variables_decl_list:
                LPAREN RPAREN { [] (* tmp *) }


program_body_statement:
                ID { Ast.EmptyStmt (* tmp *) }

program_body_statements:
                program_body_statement* { Ast.StatementList ($1) }

id_rel:         ID { $1 }
