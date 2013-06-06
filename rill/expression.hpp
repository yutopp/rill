#pragma once

#include <vector>
#include <string>
#include <memory>

#include "value.hpp"

#include "environment_fwd.hpp"


// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// expressions
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
class expression
{
public:
    virtual ~expression();

public:
    virtual value_ptr eval( const_environment_ptr const& env ) =0;
};
typedef std::shared_ptr<expression> expression_ptr;



class term_expression
    : public expression
{
public:
    term_expression( value_ptr const& v );

public:
    value_ptr eval( const_environment_ptr const& env );

private:
    value_ptr value_;
};
typedef std::shared_ptr<term_expression> term_expression_ptr;



class binary_expression
    : public expression
{
public:
    binary_expression( expression_ptr const& lhs, std::string const& op, expression_ptr const& rhs );

public:
    value_ptr eval( const_environment_ptr const& env );

private:
    expression_ptr lhs_;
    std::string op_;
    expression_ptr rhs_;
};
typedef std::shared_ptr<binary_expression> binary_expression_ptr;