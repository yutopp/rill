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
                -> decltype( std::declval<type_evaluator>().dispatch( node, env ) )
            {
                type_evaluator r;

                return r.dispatch( node, env );
            }

        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_HPP*/
