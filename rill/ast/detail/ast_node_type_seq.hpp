//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_AST_NODE_TYPE_SEQ_HPP
#define RILL_AST_DETAIL_AST_NODE_TYPE_SEQ_HPP

#define RILL_AST_NODE_TYPE_SEQ                  \
    (statement)                                 \
    (statements)                                \
    (block_statement)                           \
    (can_be_template_statement)                 \
    (template_statement)                        \
    (empty_statement)                           \
    (expression_statement)                      \
    (function_definition_statement_base)        \
    (function_definition_statement)             \
    (intrinsic_function_definition_statement)   \
    (class_function_definition_statement)       \
    (class_definition_statement)                \
    (return_statement)                          \
    (jit_statement)                             \
    (extern_statement_base)                     \
    (extern_function_declaration_statement)     \
    (variable_declaration_statement)            \
    (class_variable_declaration_statement)      \
    (test_while_statement)                      \
    (test_if_statement)                         \
                                                \
    (expression)                                \
    (binary_operator_expression)                \
    (element_selector_expression)               \
    (subscrpting_expression)                    \
    (call_expression)                           \
    (intrinsic_function_call_expression)        \
    (type_expression)                           \
    (term_expression)                           \
                                                \
    (value)                                     \
    (intrinsic::value_base)                     \
    (intrinsic::symbol_value)                   \
    (intrinsic::int32_value)                    \
    (intrinsic::boolean_value)                  \
    (intrinsic::string_value)                   \
    (intrinsic::array_value)                    \
    (identifier_value_base)                     \
    (identifier_value)                          \
    (template_instance_value)                   \
    (nested_identifier_value)                   \


#endif /*RILL_AST_DETAIL_AST_NODE_TYPE_SEQ_HPP*/
