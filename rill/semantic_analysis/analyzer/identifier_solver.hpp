//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP
#define RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP

#include <memory>

#include "../../environment/environment_kind.hpp"
#include "../../ast/value.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        // TODO: throw the exception, when failed to solve the type symbol that contained in the expression
        template<typename AnalyzerPtr, typename F>
        auto solve_type(
            AnalyzerPtr const& a,
            ast::id_expression_ptr const& id_expression,
            environment_base_ptr const& parent_env,
            F&& callback
            ) -> type_detail_ptr
        {
            std::cout << "ABA" << std::endl;

            auto const ty_detail = a->eval_type_expression_as_ctfe( id_expression, parent_env );
            auto const& ty_id = ty_detail->type_id;
            std::cout << "ABABA" << std::endl;

            auto const ty = parent_env->get_type_at( ty_id );  // copy Ty...

            // TODO: qualify by const( class_symbol_environment => class_symbol_environment const )
            auto const& class_env
                = std::static_pointer_cast<class_symbol_environment>(
                    parent_env->get_env_strong_at( ty.class_env_id )
                    );
            assert( class_env != nullptr );
            assert( class_env->get_symbol_kind() == kind::type_value::e_class );
            std::cout << "ABABA2" << std::endl;

            callback( ty_detail, ty, class_env );

            return ty_detail;
        }



    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP*/
