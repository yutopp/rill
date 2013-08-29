//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP
#define RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP

#include <memory>

#include "../../tree_visitor_base.hpp"
#include "runtime.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            class environment_constructor RILL_CXX11_FINAL
                : public tree_visitor_base<environment_ptr>
            {
            public:
                environment_constructor( context_ptr const& ctx );

            public:
                // statement_list
                RILL_TV_OP_DECL( ast::root )

                // statement
                // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

                RILL_TV_OP_DECL( ast::expression_statement )
                RILL_TV_OP_DECL( ast::return_statement )
                RILL_TV_OP_DECL( ast::function_definition_statement )
                // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

                RILL_TV_OP_DECL( ast::class_definition_statement )

                // expression
                RILL_TV_OP_DECL( ast::binary_operator_expression )
                RILL_TV_OP_DECL( ast::call_expression )
                RILL_TV_OP_DECL( ast::embedded_function_call_expression )
                RILL_TV_OP_DECL( ast::term_expression )

                //
                RILL_TV_OP_DECL( ast::intrinsic_value )
                RILL_TV_OP_DECL( ast::variable_value )

            private:
                context_ptr context_;
            };


            class type_evaluator
                : public tree_visitor_base<ast::intrinsic::identifier_value_ptr>
            {
            public:
                RILL_TV_OP_DECL( ast::type_identifier_expression )
                RILL_TV_OP_DECL( ast::compiletime_return_type_expression )
            };
        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP*/