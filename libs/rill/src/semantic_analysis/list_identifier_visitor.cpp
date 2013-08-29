//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/list_identifier_visitor.hpp>
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
        //
        void list_identifier_visitor::operator()( ast::function_definition_statement const& s, environment_ptr const& env ) const
        {
            // add function symbol to current environment
            env->pre_construct( kind::function_k, s.get_identifier()->get_last_identifier() );
        }

    } // namespace semantic_analysis
} // namespace rill