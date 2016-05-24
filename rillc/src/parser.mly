(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

%parameter <Mi : Module_info.INFO_TYPE>
%start <Ast.t> program_entry

%nonassoc IFX
%nonassoc KEYWORD_ELSE

%{
    let templatefy name node opt_tparams =
        match opt_tparams with
        | Some tparams -> Ast.TemplateStmt (name, tparams, node)
        | None -> node

    let pos s_pos e_pos =
        let open Nodes.Loc in
        let open Lexing in
        let p = {
          pos_fname         = Mi.full_filepath;
          pos_begin_cnum    = s_pos.pos_cnum;
          pos_begin_lnum    = s_pos.pos_lnum;
          pos_begin_bol     = s_pos.pos_cnum - s_pos.pos_bol;
          pos_end_cnum      = e_pos.pos_cnum;
          pos_end_lnum      = e_pos.pos_lnum;
          pos_end_bol       = e_pos.pos_cnum - e_pos.pos_bol;
        } in
        Some p
%}

%%

(**)
program_entry:
                prog_module EOF { $1 }

prog_module:
                m = module_decl
                body = top_level_statements
                {
                    let (full_base_dir, pkg_names, mod_name) = m in
                    Ast.Module (body, pkg_names, mod_name, full_base_dir, ())
                }

module_decl:
                (* empty *)
                { (Mi.full_filepath, Mi.package_names, Mi.module_name) }

        |       xs = separated_nonempty_list(DOT, rel_id_has_no_op_as_raw)
                {
                    let rev_xs = List.rev xs in
                    let pkg_names = List.rev (List.tl rev_xs) in
                    let mod_name = List.hd rev_xs in
                    (Mi.full_filepath, pkg_names, mod_name)
                }


top_level_statements:
                top_level_statement* { Ast.StatementList $1 }


(* statements *)

top_level_statement:
                attribute top_level_statement_ { Ast.AttrWrapperStmt ($1, $2) }
        |       top_level_statement_ { $1 }

top_level_statement_:
                empty_statement { $1 }
        |       function_decl_statement { $1 }
        |       class_decl_statement { $1 }
        |       extern_statement { $1 }
        |       import_statement { $1 }

empty_statement:
                SEMICOLON { Ast.EmptyStmt }

import_statement:
                KEYWORD_IMPORT
                xs = separated_nonempty_list(DOT, rel_id_has_no_op_as_raw)
                {
                    let rev_xs = List.rev xs in
                    let pkg_names = List.rev (List.tl rev_xs) in
                    let mod_name = List.hd rev_xs in
                    Ast.ImportStmt (pkg_names, mod_name, ())
                }


function_decl_statement:
                KEYWORD_DEF
                name = rel_id_as_s
                opt_tparams = template_parameter_variables_decl_list?
                params = parameter_variables_decl_list
                ret_type = type_specifier?
                t_cond = when_cond?
                body = function_decl_body_block
                {
                    let n = Ast.FunctionDefStmt (name, params, ret_type, t_cond, body, None, ()) in
                    templatefy name n opt_tparams
                }

when_cond:
                KEYWORD_WHEN expression { $2 }

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
                expr = expression
                { Ast.ReturnStmt (Some expr) }


member_function_declaration_statement:
                KEYWORD_DEF
                name = rel_id_as_s
                opt_tparams = template_parameter_variables_decl_list?
                params = parameter_variables_decl_list
                ret_type = type_specifier?
                body = function_decl_body_block
                {
                    let n = Ast.MemberFunctionDefStmt (name, params, ret_type, body, None, ()) in
                    templatefy name n opt_tparams
                }


(**)
parameter_variables_decl_list:
                LPAREN
                separated_list(COMMA, parameter_variable_declaration)
                RPAREN { Ast.ParamsList $2 }

parameter_variable_decl_introducer:
                rv = rv_attr
                mut = mut_attr
                {
                    let attr = {
                        Type_attr.ta_ref_val = rv;
                        Type_attr.ta_mut = mut;
                    } in
                    attr
                }

parameter_variable_declaration:
                parameter_variable_initializer_unit { $1 }

parameter_variable_initializer_unit:
                parameter_variable_decl_introducer
                rel_id_has_no_op_as_raw?
                value_initializer_unit { ($1, $2, $3) }


(**)
template_parameter_variables_decl_list:
                NOT
                LPAREN
                separated_nonempty_list(COMMA, template_parameter_variable_declaration)
                RPAREN { Ast.TemplateParamsList $3 }

template_parameter_variable_declaration:
                template_parameter_variable_initializer_unit { $1 }

template_parameter_variable_initializer_unit:
                rel_id_has_no_op_as_raw
                value_initializer_unit? { ($1, $2) }


(*
    = 5
    :int
    :int = 5
*)
value_initializer_unit:
                value_initializer_unit_only_value { (None, Some $1) }
        |       type_specifier value_initializer_unit_only_value? { (Some $1, $2) }


value_initializer_unit_only_value:
                ASSIGN expression { $2 }


(**)
class_decl_statement:
                class_decl_statement_ { $1 }

class_decl_statement_:
                KEYWORD_CLASS
                name = rel_id_as_s
                ml = meta_level
                body = class_decl_body_block
                { Ast.ClassDefStmt (name, body, None, () )}

class_decl_body_block:
                class_body_block    { $1 }

class_body_block:
                LBLOCK
                class_body_statements_list
                RBLOCK
                { $2 }

class_body_statement:
                attribute class_body_statement_ { Ast.AttrWrapperStmt ($1, $2) }
        |       class_body_statement_ { $1 }

class_body_statement_:
                member_variable_declaration_statement { $1 }
        |       member_function_declaration_statement { $1 }
        |       empty_statement { $1 }

class_body_statements_list:
                class_body_statement* { Ast.StatementList ($1) }


member_variable_declaration_statement:
                member_variable_declararion SEMICOLON
                {
                    Ast.MemberVariableDefStmt ($1, ())
                }

member_variable_declararion:
                member_variable_initializer_unit
                {
                    Ast.VarInit $1
                }

(* TODO: change rel_id_has_no_op_as_raw to generic_rel_id_has_no_op to support template variables *)
member_variable_initializer_unit:
                member_variable_decl_introducer
                rel_id_has_no_op_as_raw
                value_initializer_unit { ($1, $2, $3) }

member_variable_decl_introducer:
                rv = rv_attr_val
                mut = mut_attr_mutable_def
                {
                    let attr = {
                        Type_attr.ta_ref_val = rv;
                        Type_attr.ta_mut = mut;
                    } in
                    attr
                }


(**)
extern_statement:
                KEYWORD_EXTERN
                extern_statement_ { $2 }

extern_statement_:
                extern_function_statement { $1 }
        |       extern_class_statement { $1 }

extern_function_statement:
                KEYWORD_DEF
                name = rel_id_as_s
                opt_tparams = template_parameter_variables_decl_list?
                params = parameter_variables_decl_list
                ml = meta_level
                ret_type = type_specifier
                ASSIGN
                body_name = STRING (*string_lit*)
                {
                    let n = Ast.ExternFunctionDefStmt (name, params, ml, ret_type, body_name, None, ()) in
                    templatefy name n opt_tparams
                }

extern_class_statement:
                KEYWORD_CLASS
                name = rel_id_as_s
                opt_tparams = template_parameter_variables_decl_list?
                ml = meta_level
                ASSIGN
                body_name = STRING (*string_lit*)
                {
                    let n = Ast.ExternClassDefStmt (name, body_name, None, ()) in
                    templatefy name n opt_tparams
                }

return_statement:
                KEYWORD_RETURN
                e = expression?
                SEMICOLON
                { Ast.ReturnStmt e }


(**)
program_body_statement:
                attribute program_body_statement_ { Ast.AttrWrapperStmt ($1, $2) }
        |       program_body_statement_ { $1 }

program_body_statement_:
                empty_statement { $1 }
        |       expression_statement { $1 }
        |       variable_declaration_statement { $1 }
        |       return_statement { $1 }

program_body_statements_list:
                program_body_statement* { Ast.StatementList ($1) }


(**)
type_specifier:
                COLON id_expression { $2 }


(**)
rv_attr_force:
                KEYWORD_VAL { Type_attr.Val }
        |       KEYWORD_REF { Type_attr.Ref }

rv_attr_val:
                KEYWORD_VAL { Type_attr.Val }

rv_attr:
                { Type_attr.Ref }   (* default *)
        |       rv_attr_force { $1 }


mut_attr_force:
                KEYWORD_IMMUTABLE   { Type_attr.Immutable }
        |       KEYWORD_CONST       { Type_attr.Const }
        |       KEYWORD_MUTABLE     { Type_attr.Mutable }

mut_attr:
                { Type_attr.Const } (* default *)
        |       mut_attr_force { $1 }

mut_attr_mutable_def:
                { Type_attr.Mutable } (* default *)
        |       mut_attr_force { $1 }


meta_level:
                { Meta_level.Runtime }  (* default *)
        |       KEYWORD_ONLY_META       { Meta_level.OnlyMeta }
        |       KEYWORD_META            { Meta_level.Meta }
        |       KEYWORD_RUNTIME         { Meta_level.Runtime }
        |       KEYWORD_ONLY_RUNTIME    { Meta_level.OnlyRuntime }


variable_declaration_statement:
                decl = variable_declararion
                SEMICOLON
                {
                    let (vinit, ml) = decl in
                    Ast.VariableDefStmt (ml, vinit, ())
                }

(* TODO: change rel_id_has_no_op_as_raw to generic_rel_id_has_no_op to support template variables *)
variable_declararion:
                intr = variable_decl_introducer
                id = rel_id_has_no_op_as_raw
                vunit = value_initializer_unit
                {
                    let (attr, ml) = intr in
                    let vinit = Ast.VarInit (attr, id, vunit) in
                    (vinit, ml)
                }

variable_runtime_declararion:
                intr = variable_decl_runtime_introducer
                id = rel_id_has_no_op_as_raw
                vunit = value_initializer_unit
                {
                    let (attr, ml) = intr in
                    let vinit = Ast.VarInit (attr, id, vunit) in
                    (vinit, ml)
                }

variable_decl_runtime_introducer:
                rv = rv_attr_force
                mut = mut_attr
                {
                    let attr = {
                        Type_attr.ta_ref_val = rv;
                        Type_attr.ta_mut = mut;
                    } in
                    (attr, Meta_level.Runtime)  (* default metalevel *)
                }

variable_decl_meta_introducer:
                KEYWORD_META
                {
                    let attr = {
                        Type_attr.ta_ref_val = Type_attr.Val;
                        Type_attr.ta_mut = Type_attr.Immutable;
                    } in
                    (attr, Meta_level.Meta)
                }

variable_decl_introducer:
                variable_decl_runtime_introducer    { $1 }
        |       variable_decl_meta_introducer       { $1 }


(**)
argument_list:
                LPAREN
                l = separated_list(COMMA, expression)
                RPAREN { l }

template_argument_list:
                NOT
                LPAREN
                l = separated_nonempty_list(COMMA, expression)
                RPAREN
                { l }

        |       NOT
                v = primary_value
                { [v] }

(**)
expression_statement:
                expression SEMICOLON { Ast.ExprStmt $1 }


(**)
id_expression:
                logical_or_expression { $1 }


expression:
                assign_expression { $1 }

assign_expression:  (* right to left *)
                logical_or_expression { $1 }
        |       logical_or_expression op = ASSIGN assign_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       if_expression { $1 }
        |       for_expression { $1 }

if_expression:
                KEYWORD_IF
                LPAREN cond = expression RPAREN
                then_n = expression %prec IFX
                { Ast.IfExpr (cond, then_n, None, pos $startpos $endpos) }
        |       KEYWORD_IF
                LPAREN cond = expression RPAREN
                then_n = expression
                KEYWORD_ELSE
                else_n = expression
                { Ast.IfExpr (cond, then_n, Some else_n, pos $startpos $endpos) }

for_expression:
                KEYWORD_FOR
                LPAREN
                opt_decl = variable_runtime_declararion?
                SEMICOLON
                opt_cond = expression?
                SEMICOLON
                opt_inc = expression?
                RPAREN
                body = expression
                {
                    let opt_decl_stmt = match opt_decl with
                      | Some (vinit, ml) ->
                          Some (Ast.VariableDefStmt (ml, vinit, ()))
                      | None -> None
                    in
                    Ast.ForExpr (opt_decl_stmt, opt_cond, opt_inc, body)
                }

logical_or_expression:
                logical_and_expression { $1 }
        |       logical_or_expression op = LOGICAL_OR logical_and_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

logical_and_expression:
                bitwise_or_expression { $1 }
        |       logical_and_expression op = LOGICAL_AND bitwise_or_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

bitwise_or_expression:
                bitwise_xor_expression { $1 }
        |       bitwise_or_expression op = BITWISE_OR bitwise_xor_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

bitwise_xor_expression:
                bitwise_and_expression { $1 }
        |       bitwise_xor_expression op = BITWISE_XOR bitwise_and_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

bitwise_and_expression:
                equality_expression { $1 }
        |       bitwise_and_expression op = BITWISE_AND equality_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

equality_expression:
                relational_expression { $1 }
        |       equality_expression op = EQUALS relational_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       equality_expression op = NOT_EQUALS relational_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

relational_expression:
                shift_expression { $1 }
        |       relational_expression op = LTE shift_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       relational_expression op = LT shift_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       relational_expression op = GTE shift_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       relational_expression op = GT shift_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

shift_expression:
                add_sub_expression { $1 }
        |       shift_expression op = LSHIFT add_sub_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       shift_expression op = RSHIFT add_sub_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

add_sub_expression:
                mul_div_rem_expression { $1 }
        |       add_sub_expression op = PLUS mul_div_rem_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       add_sub_expression op = MINUS mul_div_rem_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

mul_div_rem_expression:
                unary_expression { $1 }
        |       mul_div_rem_expression op =TIMES unary_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       mul_div_rem_expression op =DIV unary_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }
        |       mul_div_rem_expression op = MOD unary_expression
                {
                    let op_id =
                        Ast.Id (Nodes.BinaryOp op, pos $startpos(op) $endpos(op)) in
                    Ast.BinaryOpExpr ($1, op_id, $3, pos $startpos $endpos)
                }

unary_expression:
                postfix_expression { $1 }
        |       op = MINUS postfix_expression
                {
                    let op_id =
                        Ast.Id (Nodes.UnaryPreOp op, pos $startpos(op) $endpos(op)) in
                    Ast.UnaryOpExpr (op_id, $2, pos $startpos $endpos)
                }
        |       op = INCREMENT postfix_expression
                {
                    let op_id =
                        Ast.Id (Nodes.UnaryPreOp op, pos $startpos(op) $endpos(op)) in
                    Ast.UnaryOpExpr (op_id, $2, pos $startpos $endpos)
                }
        |       op = DECREMENT postfix_expression
                {
                    let op_id =
                        Ast.Id (Nodes.UnaryPreOp op, pos $startpos(op) $endpos(op)) in
                    Ast.UnaryOpExpr (op_id, $2, pos $startpos $endpos)
                }
        |       op = NOT postfix_expression
                {
                    let op_id =
                        Ast.Id (Nodes.UnaryPreOp op, pos $startpos(op) $endpos(op)) in
                    Ast.UnaryOpExpr (op_id, $2, pos $startpos $endpos)
                }
        |       op = TIMES postfix_expression
                {
                    let op_id =
                        Ast.Id (Nodes.UnaryPreOp op, pos $startpos(op) $endpos(op)) in
                    Ast.UnaryOpExpr (op_id, $2, pos $startpos $endpos)
                }
        |       op = BITWISE_AND postfix_expression
                {
                    let op_id =
                        Ast.Id (Nodes.UnaryPreOp op, pos $startpos(op) $endpos(op)) in
                    Ast.UnaryOpExpr (op_id, $2, pos $startpos $endpos)
                }

postfix_expression:
                primary_expression { $1 }
        |       postfix_expression DOT rel_generic_id
                { Ast.ElementSelectionExpr ($1, $3, ()) }
        |       postfix_expression LBRACKET expression? RBRACKET
                { Ast.SubscriptingExpr ($1, $3) }
        |       traits_expression { $1 }
        |       call_expression { $1 }

call_expression:
                postfix_expression argument_list
                { Ast.CallExpr ($1, $2, pos $startpos $endpos) }
        |       KEYWORD_REF args = argument_list
                { Ast.TypeRVConv (Type_attr.Ref, args, pos $startpos $endpos) }
        |       KEYWORD_VAL args = argument_list
                { Ast.TypeRVConv (Type_attr.Val, args, pos $startpos $endpos) }
        |       KEYWORD_IMMUTABLE args = argument_list
                { Ast.TypeQualConv (Type_attr.Immutable, args, pos $startpos $endpos) }
        |       KEYWORD_CONST args = argument_list
                { Ast.TypeQualConv (Type_attr.Const, args, pos $startpos $endpos) }
        |       KEYWORD_MUTABLE args = argument_list
                { Ast.TypeQualConv (Type_attr.Mutable, args, pos $startpos $endpos) }
        |       KEYWORD_META args = argument_list
                { Ast.MetaLevelConv (Meta_level.Meta, args, pos $startpos $endpos) }


traits_expression:
                statement_traits_expression { $1 }

statement_traits_expression:
                KEYWORD_UU_STMT_TRAITS
                LPAREN
                t = rel_id_has_no_op_as_raw
                COMMA
                s = scope_expression
                RPAREN
                { Ast.StatementTraitsExpr (t, s) }

primary_expression:
                primary_value { $1 }
        |       LPAREN expression RPAREN { $2 }
        |       scope_expression { $1 }

scope_expression:
                LBLOCK
                stmts = program_body_statements_list
                RBLOCK
                { Ast.ScopeExpr (stmts) }

(**)
primary_value:
                boolean_literal { $1 }
        |       numeric_literal { $1 }
        |       string_literal { $1 }
        |       array_literal { $1 }
        |       generic_id { $1 }


(**)
rel_id:         rel_id_as_s { Ast.Id ($1, pos $startpos $endpos) }

rel_template_instance_id:
                rel_id_as_s template_argument_list
                { Ast.InstantiatedId ($1, $2, pos $startpos $endpos) }

rel_generic_id:
                rel_id { $1 }
        |       rel_template_instance_id { $1 }

(* TODO: implement root_generic_id *)

generic_id:
                rel_generic_id { $1 }


(**)
rel_id_has_no_op_as_raw:
                ID { $1 }

rel_id_has_no_op_as_s:
                rel_id_has_no_op_as_raw { Nodes.Pure $1 }

rel_id_as_s:
                binary_operator_as_s { $1 }
        |       unary_operator_as_s { $1 }
        |       rel_id_has_no_op_as_s { $1 }

binary_operator_as_raw:
                PLUS { $1 }
        |       MINUS { $1 }
        |       TIMES { $1 }
        |       DIV { $1 }
        |       MOD { $1 }
        |       GT { $1 }
        |       GTE { $1 }
        |       LT { $1 }
        |       LTE { $1 }
        |       LSHIFT { $1 }
        |       RSHIFT { $1 }
        |       EQUALS { $1 }
        |       NOT_EQUALS { $1 }
        |       LOGICAL_OR { $1 }
        |       LOGICAL_AND { $1 }
        |       BITWISE_AND { $1 }
        |       BITWISE_OR { $1 }
        |       BITWISE_XOR { $1 }
        |       LBRACKET RBRACKET { "[]" }

binary_operator_as_s:
                KEYWORD_OPERATOR
                op = binary_operator_as_raw
                { Nodes.BinaryOp op }

unary_operator_as_raw:
                INCREMENT { $1 }
        |       DECREMENT { $1 }

unary_pre_operator_as_raw:
                NOT { $1 }
        |       TIMES { $1 }        (* deref *)
        |       BITWISE_AND { $1 }  (* address *)

unary_post_operator_as_raw:
                LBRACKET RBRACKET { "[]" }

unary_operator_as_s:
                KEYWORD_OPERATOR KEYWORD_PRE
                op = unary_operator_as_raw
                {
                    Nodes.UnaryPreOp op
                }

        |       KEYWORD_OPERATOR KEYWORD_POST
                op = unary_operator_as_raw
                {
                    Nodes.UnaryPostOp op
                }

        |       KEYWORD_OPERATOR KEYWORD_UNARY
                op = unary_pre_operator_as_raw
                {
                    Nodes.UnaryPreOp op
                }

        |       KEYWORD_OPERATOR KEYWORD_UNARY
                op = unary_post_operator_as_raw
                {
                    Nodes.UnaryPostOp op
                }

(**)
boolean_literal:
                LIT_TRUE { Ast.BoolLit (true, pos $startpos $endpos) }
        |       LIT_FALSE { Ast.BoolLit (false, pos $startpos $endpos) }

numeric_literal:
                INT  { Ast.IntLit ($1, 32, true, pos $startpos $endpos) }
        |       UINT { Ast.IntLit ($1, 32, false, pos $startpos $endpos) }

string_literal:
                STRING { Ast.StringLit ($1, pos $startpos $endpos) }

array_literal:
                LBRACKET
                elems = separated_list(COMMA, expression)
                RBRACKET
                { Ast.ArrayLit (elems, pos $startpos $endpos) }

(**)
attribute:
                SHARP LBRACKET separated_list(COMMA, attribute_pair) RBRACKET
                {
                    let pair_list = $3 in
                    let m = Hashtbl.create (List.length pair_list) in
                    List.iter (fun (k, v) -> Hashtbl.add m k v) pair_list;
                    m
                }

attribute_pair:
                attribute_key attribute_value? { ($1, $2) }

attribute_key:
                rel_id_has_no_op_as_raw { $1 }

attribute_value:
                ASSIGN logical_or_expression { $2 }
