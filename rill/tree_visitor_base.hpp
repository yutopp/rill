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

#include "ast/value_fwd.hpp"
#include "ast/expression_fwd.hpp"
#include "ast/statement_fwd.hpp"
#include "ast/root_fwd.hpp"


namespace rill
{
    struct tree_visitor_base
    {
    public:
        virtual ~tree_visitor_base() {}

    public:
        // statement_list
        virtual void operator()( ast::root const& ss, environment_ptr const& env ) const =0;

        // statement
        // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

        virtual void operator()( ast::expression_statement const& s, environment_ptr const& env ) const =0;
        virtual void operator()( ast::return_statement const& s, environment_ptr const& env ) const =0;
        virtual void operator()( ast::function_definition_statement const& s, environment_ptr const& env ) const =0;
        // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

        virtual void operator()( ast::class_definition_statement const& s, environment_ptr const& env ) const =0;


        // expression
        virtual auto operator()( ast::binary_operator_expression const& s, environment_ptr const& env ) const -> environment_ptr =0;
        virtual auto operator()( ast::call_expression const& s, environment_ptr const& env ) const -> environment_ptr =0 ;
        virtual auto operator()( ast::embedded_function_call_expression const& s, environment_ptr const& env ) const -> environment_ptr =0;
        virtual auto operator()( ast::term_expression const& s, environment_ptr const& env ) const -> environment_ptr =0;

        // value
        virtual auto operator()( ast::intrinsic_value const& s, environment_ptr const& env ) const -> environment_ptr =0;
        virtual auto operator()( ast::variable_value const& s, environment_ptr const& env ) const -> environment_ptr =0;

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

} // namespace rill