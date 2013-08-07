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

#include "statement_fwd.hpp"

#include "value.hpp"
#include "expression.hpp"

#include "environment_fwd.hpp"
#include "tree_visitor_base.hpp"





// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// statements
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

// 
struct statement
{
    virtual ~statement()
    {};

public:
    virtual void dispatch( tree_visitor_base const&, environment_ptr const& ) const =0;
};

//
#define ADAPT_STATEMENT_VISITOR( class_name ) \
    public: \
        virtual void dispatch( tree_visitor_base const& visitor, environment_ptr const& env ) const \
        { \
            visitor( *this, env ); \
        }


//




template<typename Target>
struct template_statement
    : statement
{
    ADAPT_STATEMENT_VISITOR( template_statement )
};




struct expression_statement
    : public statement
{
    ADAPT_STATEMENT_VISITOR( expression_statement )

public:
    expression_statement( expression_ptr const& expr )
        : expression_( expr )
    {}

public:
    expression_ptr const expression_;
};





struct function_definition_statement_base
    : public statement
{
    ADAPT_STATEMENT_VISITOR( function_definition_statement_base )

public:
    function_definition_statement_base(
        intrinsic::identifier_value_ptr const& symbol_name,
        parameter_list const& parameter_list,
        intrinsic::identifier_value_ptr const& return_type
        )
        : identifier_( symbol_name )
        , parameter_list_( parameter_list )
        , return_type_( return_type )
    {}

    virtual ~function_definition_statement_base()
    {}

public:
    auto get_identifier() const
        -> intrinsic::identifier_value_ptr
    {
        return identifier_;
    }

    auto get_parameter_list() const
        -> parameter_list
    {
        return parameter_list_;
    }

public:
    intrinsic::identifier_value_ptr identifier_;
    parameter_list parameter_list_;
    intrinsic::identifier_value_ptr return_type_;
};



struct function_definition_statement
    : public function_definition_statement_base
{
    ADAPT_STATEMENT_VISITOR( function_definition_statement )

public:
    function_definition_statement(
        intrinsic::identifier_value_ptr const& symbol_name,
        parameter_list const& parameter_list,
        intrinsic::identifier_value_ptr const& return_type,
        statement_list const& statements
        )
        : function_definition_statement_base( symbol_name, parameter_list, return_type )
        , statements_( statements )
    {}

public:

public:
    statement_list const statements_;
};


struct return_statement
    : public statement
{
    ADAPT_STATEMENT_VISITOR( return_statement )

public:
    return_statement( expression_ptr const& expr )
        : expression_( expr )
    {}

public:
    expression_ptr const expression_;
};

/*

#include <functional>
typedef std::function<value_ptr(std::vector<value_ptr> const&)> native_function_t;

struct native_function_definition_statement
    : public function_definition_statement_base
{
    ADAPT_STATEMENT_VISITOR( native_function_definition_statement )

public:
    native_function_definition_statement(
        intrinsic::identifier_value_ptr const& symbol_name,
        parameter_list const& parameter_list,
        intrinsic::identifier_value_ptr const& return_type,
        native_function_t const& callee
        )
        : function_definition_statement_base( symbol_name, parameter_list, return_type )
        , callee_( callee )
    {}

public:
    native_function_t const callee_;
};
*/




struct class_definition_statement
    : public statement
{
    ADAPT_STATEMENT_VISITOR( class_definition_statement )

public:
    class_definition_statement( intrinsic::identifier_value_ptr const& identifier )
        : identifier_( identifier )
    {}

    //virtual ~class_definition_statement {}

public:
    //void setup_environment( environment_ptr const& ) const {}

    auto get_identifier() const
        -> intrinsic::identifier_value_ptr
    {
        return identifier_;
    }

private:
    intrinsic::identifier_value_ptr identifier_;
};



// make native
inline auto make_native_class( intrinsic::identifier_value_ptr const& class_name )
    -> class_definition_statement_ptr
{
    // TODO: insert assert that checks class_name depth.

    return std::make_shared<class_definition_statement>( class_name );
}











struct block_statement
    : public statement
{
    ADAPT_STATEMENT_VISITOR( block_statement )

public:
    block_statement( statement_list const& statements )
        : statements_( statements )
    {}

public:
    statement_list statements_;
};


#undef ADAPT_STATEMENT_VISITOR