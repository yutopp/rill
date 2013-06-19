//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

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
    virtual environment_ptr dispatch( tree_visitor_base const&, environment_ptr const& ) const =0;
};

//
#define ADAPT_EXPRESSION_VISITOR( class_name ) \
    public: \
        virtual environment_ptr dispatch( tree_visitor_base const& visitor, environment_ptr const& env ) const \
        { \
            return visitor( *this, env ); \
        }

typedef std::vector<expression_ptr>     expression_list;



struct binary_operator_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( binary_operator_expression )

public:
    binary_operator_expression( expression_ptr const& lhs, intrinsic::single_identifier_value_ptr const& op, expression_ptr const& rhs )
        : lhs_( lhs )
        , op_( op )
        , rhs_( rhs )
    {}

public:
    expression_ptr const lhs_;
    intrinsic::single_identifier_value_ptr const op_;
    expression_ptr const rhs_;
};


struct call_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( call_expression )

public:
    call_expression( intrinsic::identifier_value_ptr const& caller, expression_list const& arguments )
        : reciever_( caller )
        , arguments_( arguments )
    {}

public:
    intrinsic::identifier_value_ptr const reciever_;
    expression_list const arguments_;
};


//
#include <functional>
typedef std::function<intrinsic::value_base_ptr (std::vector<const_value_ptr> const&)> embedded_callback_function_t;

struct embedded_function_call_expression
    : public expression
{
    ADAPT_EXPRESSION_VISITOR( embedded_function_call_expression )

public:
    embedded_function_call_expression( embedded_callback_function_t const& reciever )
        : reciever_( reciever )
    {}

public:
    embedded_callback_function_t const reciever_;
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