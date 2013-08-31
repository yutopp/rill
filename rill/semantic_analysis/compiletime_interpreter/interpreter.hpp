//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_HPP
#define RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_HPP

#include "type_evaluator.hpp"


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

        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_HPP*/