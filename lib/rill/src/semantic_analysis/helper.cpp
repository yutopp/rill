//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // test

#include <rill/semantic_analysis/helper.hpp>

#include <rill/ast/value.hpp>

#include <rill/environment/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {

        // TODO: change name to lookup_with_template_instanciation
        auto lookup_with_instanciation(
            environment_base_ptr const& env,
            ast::const_nested_identifier_value_ptr const& ids
            )
            -> environment_base_ptr
        {
            return env->nest_lookup(
                    ids,
                    []( environment_base_ptr const& current_env, ast::const_identifier_value_base_ptr const& id ) {
                        if ( id->is_template() ) {
                            // TODO: add instatntiation
                            assert( false );
                            return nullptr;
                        } else {
                            std::cout << "[[lookup_with_instanciation]]noname ERROR!!!" << std::endl;
                            return nullptr;
                        }
                    } );
        }

        // TODO: change name to lookup_with_template_instanciation
        auto lookup_with_instanciation(
            environment_base_ptr const& env,
            ast::const_identifier_value_base_ptr const& id
            )
            -> environment_base_ptr
        {
            return env->lookup( id );
        }

    } // namespace semantic_analysis
} // namespace rill
