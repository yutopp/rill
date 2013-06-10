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



struct term_expression;
typedef std::shared_ptr<term_expression> term_expression_ptr;



struct binary_expression;
typedef std::shared_ptr<binary_expression> binary_expression_ptr;