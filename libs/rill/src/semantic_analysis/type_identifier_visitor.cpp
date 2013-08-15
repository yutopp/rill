//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/type_identifier_visitor.hpp>
#include <rill/semantic_analysis/invoke.hpp>

#include <rill/environment.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        void type_identifier_visitor::operator()( ast::root const& ss, environment_ptr const& env ) const
        {
            // ignore
        }
        
        void type_identifier_visitor::operator()( ast::expression_statement const& s, environment_ptr const& env ) const
        {
            // ignore
        }
        void type_identifier_visitor::operator()( ast::return_statement const& s, environment_ptr const& env ) const
        {
            // ignore
        }

        //
        void type_identifier_visitor::operator()( ast::function_definition_statement const& s, environment_ptr const& env ) const
        {
            // ignore
        }

        //
        void type_identifier_visitor::operator()( ast::class_definition_statement const& s, environment_ptr const& env ) const
        {
            // TODO: implement
            // ~~~~~~
        }
 

        // expression
        auto type_identifier_visitor::operator()( ast::binary_operator_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }

        auto type_identifier_visitor::operator()( ast::call_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }

        auto type_identifier_visitor::operator()( ast::embedded_function_call_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }

        auto type_identifier_visitor::operator()( ast::term_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }
        

        //
        auto type_identifier_visitor::operator()( ast::intrinsic_value const& v, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }
        auto type_identifier_visitor::operator()( ast::variable_value const& s, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }

    } // namespace semantic_analysis
} // namespace rill