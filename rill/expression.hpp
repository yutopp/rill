#pragma once

#include <vector>
#include <string>
#include "expression_fwd.hpp"

#include "value.hpp"

#include "environment_fwd.hpp"
#include "tree_visitor_base.hpp"

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// expressions
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
struct expression
{
public:
    virtual ~expression() {}

public:
    virtual value_ptr dispatch( tree_visitor_base const&, environment_ptr const& ) const =0;
};

//
#define ADAPT_EXPRESSION_VISITOR( class_name ) \
    public: \
        virtual value_ptr dispatch( tree_visitor_base const& visitor, environment_ptr const& env ) const \
        { \
            return visitor( *this, env ); \
        }

typedef std::vector<expression_ptr>     expression_list;



struct binary_operator_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( binary_operator_expression )

public:
    binary_operator_expression( expression_ptr const& lhs, literal::identifier_value_ptr const& op, expression_ptr const& rhs )
        : lhs_( lhs )
        , op_( op )
        , rhs_( rhs )
    {}

public:
    expression_ptr const lhs_;
    literal::identifier_value_ptr const op_;
    expression_ptr const rhs_;
};


struct function_call_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( function_call_expression )

public:
    function_call_expression( literal::identifier_value_ptr const& caller, expression_list const& arguments )
        : caller_( caller )
        , arguments_( arguments )
    {}

public:
    literal::identifier_value_ptr const caller_;
    expression_list const arguments_;
};


struct term_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( term_expression )

public:
    term_expression( value_ptr const& v )
        : value_( v )
    {}

public:
    value_ptr const value_;
};

#undef ADAPT_EXPRESSION_VISITOR