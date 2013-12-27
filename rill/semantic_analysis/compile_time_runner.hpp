//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_COMPILE_TIME_RUNNER_HPP
#define RILL_SEMANTIC_ANALYSIS_COMPILE_TIME_RUNNER_HPP

#include "../behavior/intrinsic_function_holder_fwd.hpp"

#include "compile_time_runner.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        // this class holds llvm_ir_generator and llvm_ir_execute_engine
        class compile_time_runner
        {
        public:
            compile_time_runner(
                intrinsic_function_action_holder_ptr const&
                ) {};
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILE_TIME_RUNNER_HPP*/
