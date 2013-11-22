//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_HPP
#define RILL_SEMANTIC_ANALYSIS_HPP

#include "compiletime_interpreter/interpreter.hpp"

#include "identifier_collector.hpp"
#include "analyzer.hpp"
#include "helper.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        // TODO: add import collector


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        auto collect_identifier( EnvironmentPtr const& env, std::vector<T> const& nodes )
            -> void
        {
            identifier_collector visitor;

            for( auto const& node : nodes )
                visitor.dispatch( node, env );
        }

        template<typename EnvironmentPtr, typename T>
        auto collect_identifier( EnvironmentPtr const& env, T const& node )
            -> void//decltype( node->dispatch_as_env( std::declval<identifier_collector>(), env ) )
        {
            identifier_collector visitor;

            return visitor.dispatch( node, env );
        }


        //
        //
        //
       template<typename EnvironmentPtr, typename T>
        auto check_and_instantiation( EnvironmentPtr const& env, T const& node )
           -> void
        {
            analyzer visitor;

            return visitor.dispatch( node, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename T>
        void analyse_and_complement( EnvironmentPtr const& env, T const& node )
        {
            analyzer visitor;

            return visitor.dispatch( node, env );
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*#include "list_identifier_visitor.hpp"*/
