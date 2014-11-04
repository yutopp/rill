//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_HPP
#define RILL_HPP

#include <tuple>
#include <memory>

#include "environment/environment.hpp"

#include "syntax_analysis/make_syntax_tree.hpp"
#include "semantic_analysis/semantic_analysis.hpp"
#include "code_generator/code_generator.hpp"

#include "behavior/intrinsic_action_holder.hpp"
#include "behavior/default_generator.hpp"


namespace rill
{
    template<typename BehaviorGenarator = behavior::default_generator>
    auto create_world()
        -> std::tuple<
            std::shared_ptr<root_environment>,
            std::shared_ptr<intrinsic_action_holder>
        >
    {
        //
        // prepareation for semantic analysis
        // it makes core.lang
        //
        auto root_env
            = std::make_shared<rill::root_environment>();
        auto intrinsic_action
            = std::make_shared<rill::intrinsic_action_holder>();

        BehaviorGenarator()( root_env, intrinsic_action );

        return std::make_tuple( std::move( root_env ), std::move( intrinsic_action ) );
    }
} // namespace rill

#endif /*RILL_HPP*/
