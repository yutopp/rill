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
            , type_detail_pool_( std::make_shared<type_detail_pool_t>() )
            , ctfe_engine_( compile_time::llvm_engine::make_ctfe_engine( this, root_env, holder, type_detail_pool_ ) )
        {}

    } // namespace semantic_analysis
} // namespace rill
