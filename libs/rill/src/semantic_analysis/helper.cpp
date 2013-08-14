//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // test

#include <rill/semantic_analysis/helper.hpp>
#include <rill/semantic_analysis/invoke.hpp>

#include <rill/ast/value.hpp>

#include <rill/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // tODO: change name to lookup_with_template_instanciation
        auto lookup_with_instanciation( environment_ptr const& env, intrinsic::const_identifier_value_ptr const& ids )
            -> environment_ptr
        {
            return env->nest_lookup(
                    ids,
                    []( environment_ptr const& current_env, intrinsic::const_single_identifier_value_base_ptr const& id ) {
                        if ( id->is_template() ) {
                            // TODO: add instatntiation
                            return nullptr;
                        } else {
                            std::cout << "[[lookup_with_instanciation]]noname ERROR!!!" << std::endl;
                            return nullptr;
                        }
                    } );
        }
    } // namespace semantic_analysis
} // namespace rill