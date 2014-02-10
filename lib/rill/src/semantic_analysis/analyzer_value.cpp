//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/analyzer_identifier_solver.hpp>

#include <rill/environment/environment.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        //
        //
        //
        RILL_TV_OP( analyzer, ast::nested_identifier_value, v, parent_env )
        {
            assert( false && "[[ICE]] not supported");
        }


        //
        //
        //
        RILL_TV_OP( analyzer, ast::identifier_value, v, parent_env )
        {
            return solve_identifier( type_detail_pool_, v, parent_env, root_env_ );
        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::template_instance_value, v, parent_env )
        {
            return solve_identifier( type_detail_pool_, v, parent_env, root_env_ );
        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::literal_value, v, env )
        {
            // look up literal type
            // TODO: look up on ROOT
            auto const class_env = env->lookup( v->literal_type_name_ );
            assert( class_env != nullptr );  // literal type must exist

            //
            return type_detail_pool_.construct(
                class_env->make_type_id( class_env, determine_type_attributes() ),
                class_env
            );
        }

    } // namespace semantic_analysis
} // namespace rill
