//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <iostream>
#include <cstddef>

#include "config/macros.hpp"

#include "environment_fwd.hpp"

#include "value_fwd.hpp"
#include "expression_fwd.hpp"
#include "statement_fwd.hpp"


struct tree_visitor_base
{
public:
    virtual ~tree_visitor_base() {}

public:
    // statement_list
    virtual void operator()( statement_list const& ss, environment_ptr const& env ) const =0;

    // statement
    // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

    virtual void operator()( expression_statement const& s, environment_ptr const& env ) const =0;
    virtual void operator()( return_statement const& s, environment_ptr const& env ) const =0;
    virtual void operator()( function_definition_statement const& s, environment_ptr const& env ) const =0;
    // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

    virtual void operator()( class_definition_statement const& s, environment_ptr const& env ) const =0;


    // expression
    virtual auto operator()( binary_operator_expression const& s, environment_ptr const& env ) const -> environment_ptr =0;
    virtual auto operator()( call_expression const& s, environment_ptr const& env ) const -> environment_ptr =0 ;
    virtual auto operator()( embedded_function_call_expression const& s, environment_ptr const& env ) const -> environment_ptr =0;
    virtual auto operator()( term_expression const& s, environment_ptr const& env ) const -> environment_ptr =0;

    // value
    virtual auto operator()( intrinsic_value const& s, environment_ptr const& env ) const -> environment_ptr =0;
    virtual auto operator()( variable_value const& s, environment_ptr const& env ) const -> environment_ptr =0;

public:
    // filter outdated object
    template<typename T>
    std::nullptr_t operator()( T const&, environment_ptr const& ) const
    {
        std::cerr
            << "DEBUG: message. please implement it!" << std::endl
            << "-> " << typeid(T).name() << std::endl;

        return nullptr;
    }
};
