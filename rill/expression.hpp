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



struct binary_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( binary_expression )

public:
    binary_expression( expression_ptr const& lhs, literal::identifier_value_ptr const& op, expression_ptr const& rhs )
        : lhs_( lhs )
        , op_( op )
        , rhs_( rhs )
    {}

public:
    expression_ptr const lhs_;
    literal::identifier_value_ptr const op_;
    expression_ptr const rhs_;
};


#undef ADAPT_EXPRESSION_VISITOR