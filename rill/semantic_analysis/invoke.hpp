//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_INVOKE_HPP
#define RILL_SEMANTIC_ANALYSIS_INVOKE_HPP

#include "list_identifier_visitor.hpp"
#include "check_and_instantiation_visitor.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        //
        //
        //
        template<typename NodePtr, typename EnvironmentPtr>
        auto list_identifier_statement( list_identifier_visitor const& r, NodePtr const& node, EnvironmentPtr const& env )
            -> decltype( node->dispatch( r, env ) )
        {
            return node->dispatch( r, env );
        }

        template<typename EnvironmentPtr, typename T>
        void list_identifier( EnvironmentPtr const& env, T const& statements )
        {
            list_identifier_visitor visitor;

            for( auto const& s : statements )
                list_identifier_statement( visitor, s, env );
        }


        //
        //
        //
        template<typename NodePtr, typename EnvironmentPtr>
        auto check_and_instantiation_statement( check_and_instantiation_visitor const& r, NodePtr const& node, EnvironmentPtr const& env )
            -> decltype( node->dispatch( r, env ) )
        {
            return node->dispatch( r, env );
        }

        template<typename EnvironmentPtr, typename T>
        void check_and_instantiation( EnvironmentPtr const& env, T const& statements )
        {
            check_and_instantiation_visitor visitor;

            for( auto const& s : statements )
                check_and_instantiation_statement( visitor, s, env );
        }


        //
        //
        //
        template<typename NodePtr, typename EnvironmentPtr>
        void analyse_statement(
            list_identifier_visitor const& lr,
            check_and_instantiation_visitor const& cr,
            NodePtr const& node, EnvironmentPtr const& env
            )
        {
            list_identifier_statement( lr, env );
            check_and_instantiation_statement( cr, env );
        }

        template<typename EnvironmentPtr, typename T>
        void analyse( EnvironmentPtr const& env, T const& statements )
        {
            list_identifier( env, statements );
            check_and_instantiation( env, statements );
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_INVOKE_HPP*/