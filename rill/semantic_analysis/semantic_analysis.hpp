//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_HPP
#define RILL_SEMANTIC_ANALYSIS_HPP

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
        auto collect_identifier( EnvironmentPtr const& env, T const& node )
            -> void//decltype( node->dispatch_as_env( std::declval<identifier_collector>(), env ) )
        {
            identifier_collector visitor;

            return visitor.dispatch( node, env );
        }


        //
        //
        //
        template<typename EnvironmentPtr, typename ActionHolderPtr, typename Node>
        void analyse_and_complement(
            EnvironmentPtr const& env,
            ActionHolderPtr const& action_holder,
            Node const& node
            )
        {
            collect_identifier( env, node );

            analyzer visitor( env, action_holder );
            return visitor.dispatch( node, env );
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_HPP*/
