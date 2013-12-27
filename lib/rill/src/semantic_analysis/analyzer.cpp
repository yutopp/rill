//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/environment/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        //
        analyzer::analyzer(
            environment_base_ptr const& root_env,
            intrinsic_function_action_holder_ptr const& holder
            )
            : root_env_( root_env )
            , runner_( std::make_shared<compile_time_runner>( holder ) )
        {}

    } // namespace semantic_analysis
} // namespace rill
