//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/identifier_collector.hpp>
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
        // Root Scope
        RILL_TV_OP( identifier_collector, ast::root, r, env )
        {
            // build environment
            for( auto const& node : r.statements_ )
                node->dispatch_as_env( *this,  env );
        }

        RILL_TV_OP( identifier_collector, ast::function_definition_statement, s, env )
        {
            // add function symbol to current environment
            env->pre_construct( kind::function_k, s.get_identifier()->get_last_identifier() );
        }

    } // namespace semantic_analysis
} // namespace rill