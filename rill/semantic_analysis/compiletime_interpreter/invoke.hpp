//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_INVOKE_HPP
#define RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_INVOKE_HPP

#include "runner.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            template<typename EnvironmentPtr, typename T>
            auto evaluate_as_type( EnvironmentPtr const& env, T const& node )
                -> decltype( node->dispatch_as_type( std::declval<type_evaluator>(), env ) )
            {
                type_evaluator r;

                return node->dispatch_as_type( r, env );
            }

            template<typename EnvironmentPtr, typename T>
            void compiletime_run_on_context( context_ptr const& ctx, EnvironmentPtr const& env, T const& node )
            {
                environment_constructor r( ctx );

                node->dispatch_as_env( r, env );
            }

            template<typename EnvironmentPtr, typename T>
            auto compiletime_run( EnvironmentPtr const& env, T const& node )
                -> std::shared_ptr<runtime>
            {
                auto const rt = std::make_shared<runtime>();

                compiletime_run_on_context( rt->create_context(), env, node );

                return rt;
            }

            //program

        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_INVOKE_HPP*/