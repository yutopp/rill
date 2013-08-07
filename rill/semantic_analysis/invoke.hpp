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
        template<typename EnvironmentPtr, typename T>
        auto list_identifier_statement( EnvironmentPtr const& env, T const& node )
            -> decltype( node->dispatch( list_identifier_visitor(), env ) )
        {
            list_identifier_visitor visitor;

            return node->dispatch( visitor, env );
        }

        template<typename EnvironmentPtr, typename T>
        void list_identifier( EnvironmentPtr const& env, std::vector<T> const& node_list )
        {
            list_identifier_visitor visitor;

            for( auto const& node : node_list )
                node->dispatch( visitor, env );
        }



        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        auto determine_parameter_signature_statement( EnvironmentPtr const& env, T const& node )
            -> bool
        {
            determine_parameter_signature_visitor visitor;
            node->dispatch( visitor, env );

            return visitor.get_solved_num() > 0;
        }

        template<typename EnvironmentPtr, typename T>
        auto determine_parameter_signature( EnvironmentPtr const& env, std::vector<T> const& node_list )
            -> std::size_t
        {
            determine_parameter_signature_visitor visitor;

            for( auto const& node : node_list )
                node->dispatch( visitor, env );

            return visitor.get_solved_num();
        }



        //
        //
        //
       template<typename EnvironmentPtr, typename T>
        auto check_and_instantiation_statement( EnvironmentPtr const& env, T const& node )
            -> decltype( node->dispatch( check_and_instantiation_visitor(), env ) )
        {
            check_and_instantiation_visitor visitor;

            return node->dispatch( visitor, env );
        }

        template<typename EnvironmentPtr, typename T>
        void check_and_instantiation( EnvironmentPtr const& env, std::vector<T> const& node_list )
        {
            check_and_instantiation_visitor visitor;

            for( auto const& node : node_list )
                node->dispatch( visitor, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        void analyse_statement( EnvironmentPtr const& env, T const& node )
        {
            list_identifier_statement( lr, env );
            determine_parameter_signature_statement
            check_and_instantiation_statement( cr, env );
        }

        template<typename EnvironmentPtr, typename T>
        void analyse( EnvironmentPtr const& env, std::vector<T> const& node_list )
        {
            list_identifier( env, node_list );
            determine_parameter_signature( env, node_list );
            check_and_instantiation( env, node_list );
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_INVOKE_HPP*/