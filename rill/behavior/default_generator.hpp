//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_DEFAULT_GENERATOR_HPP
#define RILL_BEHAVIOR_DEFAULT_GENERATOR_HPP

#include <memory>
#include <cassert>

#include "../environment/environment_fwd.hpp"
#include "intrinsic_action_holder_fwd.hpp"


namespace rill
{
    namespace behavior
    {
        void register_default_core(
            std::shared_ptr<root_environment> const&,
            std::shared_ptr<intrinsic_action_holder> const&
            );

        class default_generator
        {
        public:
            void operator()(
                std::shared_ptr<root_environment> const& root_env,
                std::shared_ptr<intrinsic_action_holder> const& intrinsic_action_holder
                ) const
            {
                register_default_core( root_env, intrinsic_action_holder );
            }
        };
    } // namespace behavior
} // namespace rill

#endif /*RILL_BEHAVIOR_DEFAULT_GENERATOR_HPP*/
