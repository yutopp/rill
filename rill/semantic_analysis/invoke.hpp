//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_INVOKE_HPP
#define RILL_SEMANTIC_ANALYSIS_INVOKE_HPP

#include <vector>

#include "type_identifier_visitor.hpp"
#include "list_identifier_visitor.hpp"
#include "check_and_instantiation_visitor.hpp"

#include "compiletime_interpreter/invoke.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        // TODO: add import collector


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        auto collect_type_identifier( EnvironmentPtr const& env, std::vector<T> const& nodes ) -> void
        {
            type_identifier_visitor visitor;

            for( auto const& node : nodes )
                node->dispatch( visitor, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        auto collect_type_identifier( EnvironmentPtr const& env, T const& node )
            -> decltype( node->dispatch( type_identifier_visitor(), env ) )
        {
            type_identifier_visitor visitor;

            return node->dispatch( visitor, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        auto collect_identifier( EnvironmentPtr const& env, std::vector<T> const& nodes ) -> void
        {
            list_identifier_visitor visitor;

            for( auto const& node : nodes )
                node->dispatch( visitor, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        auto collect_identifier( EnvironmentPtr const& env, T const& node )
            -> decltype( node->dispatch( list_identifier_visitor(), env ) )
        {
            list_identifier_visitor visitor;

            return node->dispatch( visitor, env );
        }


        //
        //
        //
       template<typename EnvironmentPtr, typename T>
        auto check_and_instantiation( EnvironmentPtr const& env, T const& node )
            -> decltype( node->dispatch( check_and_instantiation_visitor(), env ) )
        {
            check_and_instantiation_visitor visitor;

            return node->dispatch( visitor, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        void analyse( EnvironmentPtr const& env, T const& node )
        {
            interpreter::compiletime_run( env, node );
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_INVOKE_HPP*/
