//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_CHECK_AND_INSTANTIATION_VISITOR_HPP
#define RILL_SEMANTIC_ANALYSIS_CHECK_AND_INSTANTIATION_VISITOR_HPP

#include "../ast/detail/tree_visitor_base.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        class analyzer RILL_CXX11_FINAL
            : public ast::detail::tree_visitor_base<environment_ptr>
        {
        public:
            // statement_list
            RILL_TV_OP_DECL( ast::root )

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP_DECL( ast::expression_statement )
            RILL_TV_OP_DECL( ast::return_statement )
            RILL_TV_OP_DECL( ast::function_definition_statement )
            RILL_TV_OP_DECL( ast::extern_function_declaration_statement )
            //RILL_TV_OP_DECL( ast::embedded_function_definition_statement )

            RILL_TV_OP_DECL( ast::class_definition_statement )

            // expression
            RILL_TV_OP_DECL( ast::binary_operator_expression )
            RILL_TV_OP_DECL( ast::call_expression )
            //RILL_TV_OP_DECL( ast::embedded_function_call_expression )
            RILL_TV_OP_DECL( ast::term_expression )

            //
            RILL_TV_OP_DECL( ast::intrinsic_value )
            RILL_TV_OP_DECL( ast::variable_value )
        };
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_CHECK_AND_INSTANTIATION_VISITOR_HPP*/
