//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP
#define RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP

#include "../../ast/detail/tree_visitor_base.hpp"
#include "runtime.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            class type_evaluator
                : public ast::detail::tree_visitor_base<ast::intrinsic::identifier_value_ptr>
            {
            public:
                RILL_TV_OP_DECL( ast::type_identifier_expression )
                RILL_TV_OP_DECL( ast::compiletime_return_type_expression )
            };
        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP*/