%token  <int>           INT
%token  <string>        ID STRING
%token  <string>        PLUS MINUS TIMES DIV MOD
                        GT GTE LT LTE
                        LSHIFT RSHIFT
                        EQUALS NOT_EQUALS
                        ASSIGN
                        LOGICAL_OR LOGICAL_AND
                        BITWISE_AND BITWISE_OR BITWISE_XOR
                        INCREMENT DECREMENT
%token                  LPAREN RPAREN
                        LBLOCK RBLOCK
                        LBRACKET RBRACKET
%token                  COMMA DOT
                        COLON SEMICOLON
                        FAT_ARROW
%token                  EOF
%token                  LIT_TRUE LIT_FALSE
                        DECL_DEF
                        KEYWORD_VAL
                        KEYWORD_REF
                        KEYWORD_EXTERN
                        KEYWORD_OPERATOR

%start <Ast.t>          program_entry

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
        |       extern_statement { $1 }


empty_statement:
                SEMICOLON { Ast.EmptyStmt }

function_decl_statement:
                DECL_DEF
                rel_id_as_s
                parameter_variables_decl_list
                type_specifier?
                function_decl_body_block
                {
                    Ast.FunctionDefStmt ($2, $3, $4, $5, ())
                }

function_decl_body_block:
                function_body_block             { $1 }
        |       function_lambda_block SEMICOLON { $1 }

function_body_block:
                LBLOCK
                program_body_statements_list
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
                rel_id_has_no_op_as_raw? value_initializer_unit { ($1, $2) }

(*
    :int = 5
    = 5
    :int
*)
value_initializer_unit:
                value_initializer_unit_only_value { (None, Some $1) }
        |       type_specifier value_initializer_unit_only_value? { (Some $1, $2) }


value_initializer_unit_only_value:
                ASSIGN expression { $2 }


(**)
extern_statement:
                KEYWORD_EXTERN
                extern_statement_ { $2 }

extern_statement_:
                extern_function_statement { $1 }

extern_function_statement:
                DECL_DEF
                rel_id_as_s
                parameter_variables_decl_list
                type_specifier
                ASSIGN
                STRING (*string_lit*)
                { Ast.ExternFunctionDefStmt ($2, $3, $4, $6, ()) }


(**)
program_body_statement:
                (*  block_statement
                * | variable_declaration_statement
                * | control_flow_statement
                * | return_statement
                * | expression_statement
                *)
                empty_statement { $1 }
        |       expression_statement { $1 }
        |       variable_declaration_statement { $1 }

program_body_statements_list:
                program_body_statement* { Ast.StatementList ($1) }


(**)
type_specifier:
                COLON id_expression { $2 }


(**)
variable_decl_introducer:
                KEYWORD_VAL { Type.Attr.Val }
        |       KEYWORD_REF { Type.Attr.Ref }

variable_declaration_statement:
                variable_declararion SEMICOLON
                {
                    let (rv, v) = $1 in
                    Ast.VariableDefStmt (rv, Ast.VarInit v, ())
                }

variable_declararion:
                variable_decl_introducer variable_initializer_unit
                { ($1, $2) }

(* TODO: change rel_id_has_no_op_as_raw to generic_rel_id_has_no_op to support template variables *)
variable_initializer_unit:
                rel_id_has_no_op_as_raw value_initializer_unit { ($1, $2) }


(**)
argument_list:
                LPAREN
                separated_list(COMMA, assign_expression)
                RPAREN { Ast.ArgsList $2 }

(**)
expression_statement:
                expression SEMICOLON { $1 }


(**)
id_expression:
                rel_id { $1 }


expression:
                assign_expression { $1 }

assign_expression:
                conditional_expression { $1 }
        |       assign_expression ASSIGN conditional_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "=", $3) }

conditional_expression:
                logical_or_expression { $1 }

logical_or_expression:
                logical_and_expression { $1 }
        |       logical_or_expression LOGICAL_OR logical_and_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "||", $3) }

logical_and_expression:
                bitwise_or_expression { $1 }
        |       logical_and_expression LOGICAL_AND bitwise_or_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "&&", $3) }

bitwise_or_expression:
                bitwise_xor_expression { $1 }
        |       bitwise_or_expression BITWISE_OR bitwise_xor_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "|", $3) }

bitwise_xor_expression:
                bitwise_and_expression { $1 }
        |       bitwise_xor_expression BITWISE_XOR bitwise_and_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "^", $3) }

bitwise_and_expression:
                equality_expression { $1 }
        |       bitwise_and_expression BITWISE_AND equality_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "&", $3) }

equality_expression:
                relational_expression { $1 }
        |       equality_expression EQUALS relational_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "==", $3) }
        |       equality_expression NOT_EQUALS relational_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "!=", $3) }

relational_expression:
                shift_expression { $1 }
        |       relational_expression LTE shift_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "<=", $3) }
        |       relational_expression LT shift_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "<", $3) }
        |       relational_expression GTE shift_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp ">=", $3) }
        |       relational_expression GT shift_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp ">", $3) }

shift_expression:
                add_sub_expression { $1 }
        |       shift_expression LSHIFT add_sub_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "<<", $3) }
        |       shift_expression RSHIFT add_sub_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp ">>", $3) }

add_sub_expression:
                mul_div_rem_expression { $1 }
        |       add_sub_expression PLUS mul_div_rem_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "+", $3) }
        |       add_sub_expression MINUS mul_div_rem_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "-", $3) }

mul_div_rem_expression:
                unary_expression { $1 }
        |       mul_div_rem_expression TIMES unary_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "*", $3) }
        |       mul_div_rem_expression DIV unary_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "/", $3) }
        |       mul_div_rem_expression MOD unary_expression
                { Ast.BinaryOpExpr ($1, Nodes.BinaryOp "%", $3) }

unary_expression:
                postfix_expression { $1 }
        |       MINUS postfix_expression { Ast.UnaryOpExpr("-", $2) }


postfix_expression:
                primary_expression { $1 }
        |       postfix_expression DOT rel_generic_id
                { Ast.ElementSelectionExpr ($1, $3) }
        |       postfix_expression LBRACKET expression? RBRACKET
                { Ast.SubscriptingExpr ($1, $3) }
        |       postfix_expression argument_list
                { Ast.CallExpr ($1, $2) }

primary_expression:
                primary_value { $1 }
        |       LPAREN expression RPAREN { $2 }


(**)
primary_value:
                boolean_literal { $1 }
        |       numeric_literal { $1 }
        |       string_literal { $1 }
        |       generic_id { $1 }


(**)

binary_operator_as_raw:
                PLUS { $1 }

binary_operator_as_s:
                KEYWORD_OPERATOR
                binary_operator_as_raw { Nodes.BinaryOp ($2) }

unary_operator_as_raw:
                INCREMENT { $1 }
        |       DECREMENT { $1 }

unary_operator_as_s:
                KEYWORD_OPERATOR
                unary_operator_as_raw ID
                {
                    match $3 with
                    | "pre" -> Nodes.UnaryPreOp ($2)
                    | "post" -> Nodes.UnaryPostOp ($2)
                    | _ -> failwith "~~~"
                }

rel_id_has_no_op_as_raw:
                ID { $1 }

rel_id_has_no_op_as_s:
                rel_id_has_no_op_as_raw { Nodes.Pure ($1) }

rel_id_as_s:
                binary_operator_as_s { $1 }
        |       unary_operator_as_s { $1 }
        |       rel_id_has_no_op_as_s { $1 }


(*
rel_id_has_no_op:
                rel_id_has_no_op_as_s { Ast.Id ($1) }
*)
rel_id:         rel_id_as_s { Ast.Id ($1) }


rel_generic_id:
                rel_id { $1 }

generic_id:
                rel_id { $1 }


(**)
boolean_literal:
                LIT_TRUE { Ast.BoolLit (true) }
        |       LIT_FALSE { Ast.BoolLit (false) }

numeric_literal:
                INT { Ast.Int32Lit ($1) }

string_literal:
                STRING { Ast.StringLit ($1) }
