#pragma once

#include <memory>

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// expressions
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
struct expression;
typedef std::shared_ptr<expression> expression_ptr;

struct binary_operator_expression;
typedef std::shared_ptr<binary_operator_expression> binary_operator_expression_ptr;

struct call_expression;
typedef std::shared_ptr<call_expression> call_expression_ptr;

struct embedded_function_call_expression;
typedef std::shared_ptr<embedded_function_call_expression> embedded_function_call_expression_ptr;

struct term_expression;
typedef std::shared_ptr<term_expression> term_expression_ptr;