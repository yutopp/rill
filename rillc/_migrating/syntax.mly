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
  let to_loc s_pos e_pos =
    let open Loc in
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

  let make ~b ~e kind =
    let loc = to_loc b e in
    Ast.{kind; loc}

  let wrap kind ~base =
    let loc = Ast.loc_of base in
    Ast.{kind; loc}

  let templatefy name node opt_tparams =
    match opt_tparams with
    | Some tparams -> wrap (Ast.TemplateStmt (name, tparams, node)) ~base:node
    | None -> node
%}

%%

(**)
program_entry:
    prog_module EOF { $1 }

prog_module:
    m = module_decl
    body = top_level_statements
    {
      let (pkg_names, mod_name, attr_opt) = m in
      let node =
        make (Ast.Module (body, pkg_names, mod_name, Mi.full_filepath, ()))
             ~b:$startpos
             ~e:$endpos
      in
      match attr_opt with
      | Some attr -> wrap (Ast.AttrWrapperStmt (attr, node)) ~base:node
      | None -> node
    }

%inline
module_decl:
    (* no module specifier *)
    {
      let pkg_names = Mi.package_names in
      let mod_name = Mi.module_name in
      (pkg_names, mod_name, None)
    }

  | attr_opt = attribute?
    KEYWODD_PACKAGE
    xs = separated_nonempty_list(DOT, rel_id_has_no_op_as_raw)
    {
      let pkg_names = List.rev xs in
      let mod_name = Mi.module_name in
      (pkg_names, mod_name, attr_opt)
    }

  | attr_opt = attribute?
    KEYWODD_MODULE
    xs = separated_nonempty_list(DOT, rel_id_has_no_op_as_raw)
    {
      let rev_xs = List.rev xs in
      let pkg_names = List.rev (List.tl rev_xs) in
      let mod_name = List.hd rev_xs in
      (pkg_names, mod_name, attr_opt)
    }


top_level_statements:
    top_level_statement*
    {
      make (Ast.StatementList $1)
           ~b:$startpos
           ~e:$endpos
    }


(* statements *)

top_level_statement:
    attribute top_level_statement_ { wrap (Ast.AttrWrapperStmt ($1, $2)) ~base:$2 }
  | top_level_statement_ { $1 }

top_level_statement_:
    empty_statement { $1 }
  | function_decl_statement { $1 }
  | class_decl_statement { $1 }
  | extern_statement { $1 }
  | import_statement { $1 }

empty_statement:
    SEMICOLON { make Ast.EmptyStmt ~b:$startpos ~e:$endpos }

import_statement:
    opt_public = KEYWORD_PUBLIC?
    KEYWORD_IMPORT
    xs = separated_nonempty_list(DOT, rel_id_has_no_op_as_raw)
    {
      let rev_xs = List.rev xs in
      let pkg_names = List.rev (List.tl rev_xs) in
      let mod_name = List.hd rev_xs in
      let is_public = match opt_public with
        | Some _ -> true
        | None -> false
      in
      make (Ast.ImportStmt (pkg_names, mod_name, is_public, ()))
           ~b:$startpos
           ~e:$endpos
    }

function_decl_statement:
    KEYWORD_DEF
    lifetimes = lifetime_parameter_decl_list
    name = rel_id_as_s
    opt_tparams = template_parameter_variables_decl_list
    params = parameter_variables_decl_list
    opt_ret_type = type_specifier?
    ml = meta_level
    t_cond = when_cond?
    body = function_decl_body_block
    {
      let node =
        make (Ast.FunctionDefStmt (name, lifetimes, params, ml, opt_ret_type, t_cond, body, None, ()))
             ~b:$startpos
             ~e:$endpos
      in
      templatefy name node opt_tparams
    }

when_cond:
    KEYWORD_WHEN logical_or_expression { $2 }

function_decl_body_block:
    function_body_block             { $1 }
  | function_lambda_block SEMICOLON { $1 }

function_body_block:
    LBLOCK
    program_body_statements_list
    RBLOCK
    { $2 }

function_lambda_block:
    FAT_ARROW
    expr = expression
    {
      make (Ast.ReturnStmt (Some expr)) ~b:$startpos ~e:$endpos
    }


member_function_declaration_statement:
    KEYWORD_DEF
    lifetimes = lifetime_parameter_decl_list
    name = rel_id_as_s
    opt_tparams = template_parameter_variables_decl_list
    params = parameter_variables_decl_list
    quals = qual_spec_list
    ret_type = type_specifier?
    body = function_decl_body_block
    {
      let node =
        make (Ast.MemberFunctionDefStmt (name, lifetimes, params, quals, ret_type, body, None, ()))
             ~b:$startpos
             ~e:$endpos
      in
      templatefy name node opt_tparams
    }


(**)
parameter_variables_decl_list:
    LPAREN
    separated_list(COMMA, parameter_variable_declaration)
    RPAREN
    { make (Ast.ParamsList $2) ~b:$startpos ~e:$endpos }

parameter_variable_decl_introducer:
    rv = rv_attr
    mut = mut_attr
    {
      let attr = Type_attr.{
        ta_ref_val = rv;
        ta_mut = mut;
      } in
      attr
    }

parameter_variable_declaration:
    parameter_variable_initializer_unit { $1 }

parameter_variable_initializer_unit:
    parameter_variable_decl_introducer
    rel_id_has_no_op_as_s?
    value_initializer_unit
    { ($1, $2, $3) }


(**)
lifetime_parameter_decl_list:
    { [] }

  | ps = lifetime_single_param
    { ps }

  | LPAREN
    ps = separated_nonempty_list(COMMA, lifetime_param)
    RPAREN
    { ps }

lifetime_var:
    SINGLEQUOTE
    id = ID
    { Id_string.Pure ("`" ^ id) }
  | SINGLEQUOTE
    KEYWORD_STATIC
    { Id_string.Pure ("`static") }
  | SINGLEQUOTE
    KEYWORD_GC
    { Id_string.Pure ("`gc") }
  | SINGLEQUOTE
    KEYWORD_UNMANAGED
    { Id_string.Pure ("`unmanaged") }

lifetime_single_arg:
    id = lifetime_var
    { [id] }

lifetime_single_param:
    id = lifetime_var
    { [Lifetime.LtSingle (id, to_loc $startpos(id) $endpos(id))] }

lifetime_param:
    lifetime_var { Lifetime.LtSingle ($1, to_loc $startpos($1) $endpos($1)) }
    (* constraint *)
  | lifetime_var COLON lifetime_var { Lifetime.LtLongerThan ($1, $3) }

lifetime_arg_list2:
    id = lifetime_var
    { [id] }
  | LPAREN
    ids = separated_nonempty_list(COMMA, lifetime_var)
    RPAREN
    { ids }

(**)
qual_spec_list:
    list(qual_spec)     { $1 }

qual_spec:
    KEYWORD_MUTABLE     { Nodes.QualMutable }
  | KEYWORD_CONST       { Nodes.QualConst }
  | KEYWORD_IMMUTABLE   { Nodes.QualImmutable }

(**)
template_parameter_variables_decl_list:
    { None }
  | NOT
    template_parameter_variables_decl_list_impl
    { (Some $2) }

template_parameter_variables_decl_list_impl:
    LPAREN
    separated_list(COMMA, template_parameter_variable_declaration)
    RPAREN
    { make (Ast.TemplateParamsList $2) ~b:$startpos ~e:$endpos }

template_parameter_variable_declaration:
    template_parameter_variable_initializer_unit { $1 }

template_parameter_variable_initializer_unit:
    rel_id_has_no_op_as_s
    value_initializer_unit? { ($1, $2) }


(*
    = 5
    :int
    :int = 5
*)
value_initializer_unit:
    value_initializer_unit_only_value { (None, Some $1) }
  | type_specifier value_initializer_unit_only_value? { (Some $1, $2) }


value_initializer_unit_only_value:
    ASSIGN expression { $2 }


(**)
class_decl_statement:
    class_decl_statement_ { $1 }

class_decl_statement_:
    KEYWORD_CLASS
    lifetimes = lifetime_parameter_decl_list
    name = rel_id_as_s
    opt_tparams = template_parameter_variables_decl_list
    ml = meta_level
    body = class_decl_body_block
    {
      let node =
        make (Ast.ClassDefStmt (name, lifetimes, body, None, ()))
             ~b:$startpos
             ~e:$endpos
      in
      templatefy name node opt_tparams
    }

class_decl_body_block:
    class_body_block    { $1 }

class_body_block:
    LBLOCK
    class_body_statements_list
    RBLOCK
    { $2 }

class_body_statement:
    attribute class_body_statement_
    { make (Ast.AttrWrapperStmt ($1, $2)) ~b:$startpos ~e:$endpos }
  | class_body_statement_
    { $1 }

class_body_statement_:
    member_variable_declaration_statement { $1 }
  | member_function_declaration_statement { $1 }
  | empty_statement { $1 }

class_body_statements_list:
    class_body_statement* { make (Ast.StatementList ($1)) ~b:$startpos ~e:$endpos }


member_variable_declaration_statement:
    member_variable_declararion SEMICOLON
    {
      make (Ast.MemberVariableDefStmt ($1, ())) ~b:$startpos ~e:$endpos
    }

member_variable_declararion:
    member_variable_initializer_unit
    {
      make (Ast.VarInit $1) ~b:$startpos ~e:$endpos
    }

(* TODO: change rel_id_has_no_op_as_raw to generic_rel_id_has_no_op to support template variables *)
member_variable_initializer_unit:
    member_variable_decl_introducer
    rel_id_has_no_op_as_s
    value_initializer_unit
    { ($1, $2, $3) }

member_variable_decl_introducer:
    rv = rv_attr_val
    mut = mut_attr_mutable_def
    {
      let attr = Type_attr.{
          ta_ref_val = rv;
          ta_mut = mut;
      } in
      attr
    }

(**)
extern_statement:
    KEYWORD_EXTERN extern_statement_ { $2 }

extern_statement_:
    extern_function_statement { $1 }
  | extern_class_statement { $1 }

extern_function_statement:
    KEYWORD_DEF
    lifetimes = lifetime_parameter_decl_list
    name = rel_id_as_s
    opt_tparams = template_parameter_variables_decl_list
    params = parameter_variables_decl_list
    ml = meta_level
    ret_type = type_specifier
    t_cond = when_cond?
    ASSIGN
    body_name = STRING (*string_lit*)
    {
      let node =
        make (Ast.ExternFunctionDefStmt (name, lifetimes, params, ml, ret_type, t_cond, body_name, None, ()))
             ~b:$startpos
             ~e:$endpos
      in
      templatefy name node opt_tparams
    }

extern_class_statement:
    KEYWORD_CLASS
    lifetimes = lifetime_parameter_decl_list
    name = rel_id_as_s
    opt_tparams = template_parameter_variables_decl_list
    ml = meta_level
    ASSIGN
    body_name = STRING (*string_lit*)
    opt_body = class_decl_body_block?
    {
      let node =
        make (Ast.ExternClassDefStmt (name, lifetimes, body_name, opt_body, None, ()))
             ~b:$startpos
             ~e:$endpos
      in
      templatefy name node opt_tparams
    }

return_statement:
    KEYWORD_RETURN
    e = expression?
    SEMICOLON
    { make (Ast.ReturnStmt e) ~b:$startpos ~e:$endpos }


(**)
program_body_statement:
    attribute program_body_statement_
    { make (Ast.AttrWrapperStmt ($1, $2)) ~b:$startpos ~e:$endpos }
  | program_body_statement_ { $1 }

program_body_statement_p_(ExpStmt):
    empty_statement { $1 }
  | control_flow_statement { $1 }
  | scope_statement { $1 }
  | ExpStmt { $1 }
  | variable_declaration_statement { $1 }
  | return_statement { $1 }

program_body_statement_:
    program_body_statement_p_(expression_statement) { $1 }

program_body_statements_list:
    program_body_statement* { make (Ast.StatementList ($1)) ~b:$startpos ~e:$endpos }


(**)
type_specifier:
    COLON id_expression { $2 }


(**)
rv_attr_force:
    KEYWORD_VAL { Type_attr.Val }
  | KEYWORD_REF { Type_attr.Ref [] }
  | lts = lifetime_single_arg KEYWORD_REF { Type_attr.Ref lts }

rv_attr_val:
    KEYWORD_VAL { Type_attr.Val }

rv_attr:
    { Type_attr.Ref [] }   (* default *)
  | rv_attr_force { $1 }


mut_attr_force:
    KEYWORD_IMMUTABLE   { Type_attr.Immutable }
  | KEYWORD_CONST       { Type_attr.Const }
  | KEYWORD_MUTABLE     { Type_attr.Mutable }

mut_attr:
    { Type_attr.Const } (* default *)
  | mut_attr_force { $1 }

mut_attr_mutable_def:
    { Type_attr.Mutable } (* default *)
  | mut_attr_force { $1 }


meta_level:
    { Meta_level.Meta }     (* default *)
  | KEYWORD_ONLY_META       { Meta_level.OnlyMeta }
  | KEYWORD_META            { Meta_level.Meta }
  | KEYWORD_RUNTIME         { Meta_level.Runtime }
  | KEYWORD_ONLY_RUNTIME    { Meta_level.OnlyRuntime }


variable_declaration_statement:
    decl = variable_declararion
    SEMICOLON
    {
      let (vinit, ml) = decl in
      make (Ast.VariableDefStmt (ml, vinit, ())) ~b:$startpos ~e:$endpos
    }

(* TODO: change rel_id_has_no_op_as_s to generic_rel_id_has_no_op to support template variables *)
variable_declararion:
    intr = variable_decl_introducer
    id = rel_id_has_no_op_as_s
    vunit = value_initializer_unit
    {
      let (attr, ml) = intr in
      let node =
        make (Ast.VarInit (attr, id, vunit)) ~b:$startpos ~e:$endpos
      in
      (node, ml)
    }

variable_runtime_declararion:
    intr = variable_decl_runtime_introducer
    id = rel_id_has_no_op_as_s
    vunit = value_initializer_unit
    {
      let (attr, ml) = intr in
      let vinit = make (Ast.VarInit (attr, id, vunit)) ~b:$startpos ~e:$endpos in
      (vinit, ml)
    }

variable_decl_runtime_introducer:
    rv = rv_attr_force
    mut = mut_attr
    {
      let attr = Type_attr.{
        ta_ref_val = rv;
        ta_mut = mut;
      } in
      (attr, Meta_level.Runtime)  (* default metalevel *)
    }

variable_decl_meta_introducer:
    KEYWORD_META
    {
      let attr = Type_attr.{
        ta_ref_val = Type_attr.Val;
        ta_mut = Type_attr.Immutable;
      } in
      (attr, Meta_level.Meta)
    }

variable_decl_introducer:
    variable_decl_runtime_introducer    { $1 }
  | variable_decl_meta_introducer       { $1 }


(**)
argument_list:
    LPAREN
    l = separated_list(COMMA, expression)
    RPAREN { l }

template_argument_list:
    LPAREN
    l = separated_list(COMMA, expression)
    RPAREN
    { l }

  | v = template_primary_value
    { [v] }

(**)
control_flow_statement:
    if_statement { make (Ast.ExprStmt $1) ~b:$startpos ~e:$endpos }

if_statement:
    KEYWORD_IF
    LPAREN cond = assign_expression RPAREN
    then_n = if_clause_statement %prec IFX
    {
      let then_void_stmt = wrap (Ast.VoidExprStmt then_n) ~base:then_n in
      let then_ast = wrap (Ast.ScopeExpr then_void_stmt) ~base:then_void_stmt in
      make (Ast.IfExpr (cond, then_ast, None, ())) ~b:$startpos ~e:$endpos
    }
  | KEYWORD_IF
    LPAREN cond = assign_expression RPAREN
    then_n = if_clause_statement
    KEYWORD_ELSE
    else_n = if_clause_statement
    {
      let then_void_stmt = wrap (Ast.VoidExprStmt then_n) ~base:then_n in
      let then_ast = wrap (Ast.ScopeExpr then_void_stmt) ~base:then_void_stmt in
      let else_void_stmt = wrap (Ast.VoidExprStmt else_n) ~base:else_n in
      let else_ast = wrap (Ast.ScopeExpr else_void_stmt) ~base:else_void_stmt in
      make (Ast.IfExpr (cond, then_ast, Some else_ast, ())) ~b:$startpos ~e:$endpos
    }

if_clause_statement:
    program_body_statement_p_(assign_expression_statement) { $1 }

scope_statement:
    scope_expression { make (Ast.ExprStmt $1) ~b:$startpos ~e:$endpos }

(**)
expression_statement:
    expression SEMICOLON { make (Ast.ExprStmt $1) ~b:$startpos ~e:$endpos }

%inline
assign_expression_statement:
    assign_expression SEMICOLON { make (Ast.ExprStmt $1) ~b:$startpos ~e:$endpos }

(**)
id_expression:
    logical_or_expression { $1 }

expression:
    control_flow_expression { $1 }

control_flow_expression:
    assign_expression { $1 }
  | if_expression { $1 }
  | for_expression { $1 }
  | KEYWORD_WITH scope_expression { $2 }

if_expression:
    KEYWORD_IF
    LPAREN cond = assign_expression RPAREN
    then_n = expression %prec IFX
    { make (Ast.IfExpr (cond, then_n, None, ())) ~b:$startpos ~e:$endpos }
  | KEYWORD_IF
    LPAREN cond = assign_expression RPAREN
    then_n = expression
    KEYWORD_ELSE
    else_n = expression
    { make (Ast.IfExpr (cond, then_n, Some else_n, ())) ~b:$startpos ~e:$endpos }

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
           Some (make (Ast.VariableDefStmt (ml, vinit, ())) ~b:$startpos ~e:$endpos)
        | None -> None
      in
      make (Ast.ForExpr (opt_decl_stmt, opt_cond, opt_inc, body)) ~b:$startpos ~e:$endpos
    }

scope_expression:
    LBLOCK
    stmts = program_body_statements_list
    (*expr = option(expression)*)
    RBLOCK
    { make (Ast.ScopeExpr (stmts)) ~b:$startpos ~e:$endpos }

assign_expression:  (* right to left *)
    logical_or_expression { $1 }
  | logical_or_expression op = ASSIGN assign_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

logical_or_expression:
    logical_and_expression { $1 }
  | logical_or_expression op = LOGICAL_OR logical_and_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

logical_and_expression:
    bitwise_or_expression { $1 }
  | logical_and_expression op = LOGICAL_AND bitwise_or_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

bitwise_or_expression:
    bitwise_xor_expression { $1 }
  | bitwise_or_expression op = BITWISE_OR bitwise_xor_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

bitwise_xor_expression:
    bitwise_and_expression { $1 }
  | bitwise_xor_expression op = BITWISE_XOR bitwise_and_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

bitwise_and_expression:
    equality_expression { $1 }
  | bitwise_and_expression op = BITWISE_AND equality_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

equality_expression_op:
    EQUALS     { $1 }
  | NOT_EQUALS { $1 }

equality_expression:
    relational_expression { $1 }
  | equality_expression op = equality_expression_op relational_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

relational_expression_op:
    LTE { $1 }
  | LT  { $1 }
  | GTE { $1 }
  | GT  { $1 }

relational_expression:
    shift_expression { $1 }
  | relational_expression op = relational_expression_op shift_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

shift_expression_op:
    LSHIFT  { $1 }
  | ARSHIFT { $1 }
  | LRSHIFT { $1 }

shift_expression:
    add_sub_expression { $1 }
  | shift_expression op = shift_expression_op add_sub_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

add_sub_expression_op:
    PLUS  { $1 }
  | MINUS { $1 }

add_sub_expression:
    mul_div_rem_expression { $1 }
  | add_sub_expression op = add_sub_expression_op mul_div_rem_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

mul_div_rem_expression_op:
    TIMES { $1 }
  | DIV   { $1 }
  | MOD   { $1 }
  ;
mul_div_rem_expression:
    unary_expression { $1 }
  | mul_div_rem_expression op = mul_div_rem_expression_op unary_expression
    {
      let op_name = Id_string.BinaryOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.BinaryOpExpr ($1, op_id, $3, ())) ~b:$startpos ~e:$endpos
    }

unary_expression_op:
    MINUS       { $1 }
  | INCREMENT   { $1 }
  | DECREMENT   { $1 }
  | NOT         { $1 }
  | TIMES       { $1 }
  | BITWISE_AND { $1 }

unary_expression:
    postfix_expression { $1 }
  | op = unary_expression_op postfix_expression
    {
      let op_name = Id_string.UnaryPreOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.UnaryOpExpr (op_id, $2, ())) ~b:$startpos ~e:$endpos
    }
  | type_qual_expression { $1 }
  | meta_spec_expression { $1 }

%inline
type_qual_expression:
    KEYWORD_REF args = argument_list
    { make (Ast.TypeRVConv (Type_attr.Ref [], args, ())) ~b:$startpos ~e:$endpos }
  | lts = lifetime_single_arg KEYWORD_REF args = argument_list
    { make (Ast.TypeRVConv (Type_attr.Ref lts, args, ())) ~b:$startpos ~e:$endpos }
  | KEYWORD_VAL args = argument_list
    { make (Ast.TypeRVConv (Type_attr.Val, args, ())) ~b:$startpos ~e:$endpos }
  | KEYWORD_IMMUTABLE args = argument_list
    { make (Ast.TypeQualConv (Type_attr.Immutable, args, ())) ~b:$startpos ~e:$endpos }
  | KEYWORD_CONST args = argument_list
    { make (Ast.TypeQualConv (Type_attr.Const, args, ())) ~b:$startpos ~e:$endpos }
  | KEYWORD_MUTABLE args = argument_list
    { make (Ast.TypeQualConv (Type_attr.Mutable, args, ())) ~b:$startpos ~e:$endpos }

%inline
meta_spec_expression:
  | KEYWORD_META args = argument_list
    {
      make (Ast.MetaLevelConv (Meta_level.Meta, args, ())) ~b:$startpos ~e:$endpos
    }

postfix_expression:
    primary_expression { $1 }
  | postfix_expression op = unary_operator_as_raw
    {
      let op_name = Id_string.UnaryPostOp op in
      let op_id = make (Ast.Id (op_name, [], ())) ~b:$startpos(op) ~e:$endpos(op) in
      make (Ast.UnaryOpExpr (op_id, $1, ())) ~b:$startpos ~e:$endpos
    }
  | postfix_expression DOT rel_generic_id
    { make (Ast.ElementSelectionExpr ($1, $3, ())) ~b:$startpos($3) ~e:$endpos($3) }
  | postfix_expression LBRACKET expression? RBRACKET
    { make (Ast.SubscriptingExpr ($1, $3, ())) ~b:$startpos($2) ~e:$endpos($4) }
  | traits_expression { $1 }
  | call_expression { $1 }

%inline
call_expression:
    postfix_expression argument_list
    { make (Ast.CallExpr ($1, $2, ())) ~b:$startpos ~e:$endpos }

traits_expression:
    statement_traits_expression { $1 }

statement_traits_expression:
    KEYWORD_UU_STMT_TRAITS
    LPAREN
    t = rel_id_has_no_op_as_raw
    COMMA
    s = scope_expression
    RPAREN
    { make (Ast.StatementTraitsExpr (t, s)) ~b:$startpos ~e:$endpos }

primary_expression:
    primary_value { $1 }
  | LPAREN expression RPAREN { $2 }

(**)
primary_value:
    boolean_literal { $1 }
  | numeric_literal { $1 }
  | string_literal { $1 }
  | array_literal { $1 }
  | generic_id { $1 }

template_primary_value:
    boolean_literal { $1 }
  | numeric_literal { $1 }
  | string_literal { $1 }
  | array_literal { $1 }
  | rel_id { $1 }

(**)
rel_id:
    rel_id_as_s { make (Ast.Id ($1, [], ())) ~b:$startpos ~e:$endpos }

rel_generics_id:
    rel_id
    { $1 }
  | lts = lifetime_arg_list2 id = rel_id_as_s
    { make (Ast.Id (id, lts, ())) ~b:$startpos ~e:$endpos }

rel_template_instance_id:
    rel_id_as_s NOT template_argument_list
    { make (Ast.InstantiatedId ($1, $3, [], ())) ~b:$startpos ~e:$endpos }
  | lts = lifetime_arg_list2 id = rel_id_as_s NOT ts = template_argument_list
    { make (Ast.InstantiatedId (id, ts, lts, ())) ~b:$startpos ~e:$endpos }

rel_generic_id:
    rel_generics_id { $1 }
  | rel_template_instance_id { $1 }

(* TODO: implement root_generic_id *)

generic_id:
    rel_generic_id { $1 }


(**)
rel_id_has_no_op_as_raw:
    ID { $1 }

rel_id_has_no_op_as_s:
    rel_id_has_no_op_as_raw { Id_string.Pure $1 }

rel_id_as_s:
    binary_operator_as_s { $1 }
  | unary_operator_as_s { $1 }
  | rel_id_has_no_op_as_s { $1 }

%inline
binary_operator_as_raw:
    PLUS { $1 }
  | MINUS { $1 }
  | TIMES { $1 }
  | DIV { $1 }
  | MOD { $1 }
  | ASSIGN { $1 }
  | GT { $1 }
  | GTE { $1 }
  | LT { $1 }
  | LTE { $1 }
  | LSHIFT { $1 }
  | ARSHIFT { $1 }
  | LRSHIFT { $1 }
  | EQUALS { $1 }
  | NOT_EQUALS { $1 }
  | LOGICAL_OR { $1 }
  | LOGICAL_AND { $1 }
  | BITWISE_AND { $1 }
  | BITWISE_OR { $1 }
  | BITWISE_XOR { $1 }
  | LBRACKET RBRACKET { "[]" }

%inline
binary_operator_as_s:
    KEYWORD_OPERATOR
    op = binary_operator_as_raw
    { Id_string.BinaryOp op }

%inline
unary_operator_as_raw:
    INCREMENT { $1 }
  | DECREMENT { $1 }

%inline
unary_pre_operator_as_raw:
    NOT { $1 }
  | MINUS { $1 }
  | TIMES { $1 }        (* deref *)
  | BITWISE_AND { $1 }  (* address *)

unary_post_operator_as_raw:
    LBRACKET RBRACKET { "[]" }

unary_operator_as_s:
    KEYWORD_OPERATOR KEYWORD_PRE
    op = unary_operator_as_raw
    {
      Id_string.UnaryPreOp op
    }

  | KEYWORD_OPERATOR KEYWORD_POST
    op = unary_operator_as_raw
    {
      Id_string.UnaryPostOp op
    }

  | KEYWORD_OPERATOR KEYWORD_UNARY
    op = unary_pre_operator_as_raw
    {
      Id_string.UnaryPreOp op
    }

  | KEYWORD_OPERATOR KEYWORD_UNARY
    op = unary_post_operator_as_raw
    {
      Id_string.UnaryPostOp op
    }

(**)
boolean_literal:
    LIT_TRUE { make (Ast.BoolLit (true, ())) ~b:$startpos ~e:$endpos }
  | LIT_FALSE { make (Ast.BoolLit (false, ())) ~b:$startpos ~e:$endpos }

numeric_literal:
    INT
    {
      let (v, bits, signed) = $1 in
      make (Ast.IntLit (v, bits, signed, ())) ~b:$startpos ~e:$endpos
    }

string_literal:
    STRING { make (Ast.StringLit ($1, ())) ~b:$startpos ~e:$endpos }

array_literal:
    LBRACKET
    elems = separated_list(COMMA, expression)
    RBRACKET
    { make (Ast.ArrayLit (elems, false, ())) ~b:$startpos ~e:$endpos }

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
