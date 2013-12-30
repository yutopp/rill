//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP
#define RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP

#include <memory>

#include "../ast/detail/tree_visitor_base.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"
#include "../compile_time/llvm_engine.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        class analyzer RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<analyzer, type_id_with_env>
        {
        public:
            analyzer(
                environment_base_ptr const&,
                intrinsic_function_action_holder_ptr const&,
                std::shared_ptr<compile_time::llvm_engine const> const&
                );

        public:
            // statement
            RILL_TV_OP_DECL( ast::statements )
            RILL_TV_OP_DECL( ast::block_statement )
          // RILL_TV_OP_DECL( ast::template_statement )
            RILL_TV_OP_DECL( ast::expression_statement )
            RILL_TV_OP_DECL( ast::return_statement )
            RILL_TV_OP_DECL( ast::jit_statement )
            RILL_TV_OP_DECL( ast::function_definition_statement )
            RILL_TV_OP_DECL( ast::variable_declaration_statement )
          //RILL_TV_OP_DECL( ast::intrinsic_function_definition_statement )
            RILL_TV_OP_DECL( ast::extern_function_declaration_statement )
            RILL_TV_OP_DECL( ast::class_definition_statement )
            RILL_TV_OP_DECL( ast::class_function_definition_statement )
            RILL_TV_OP_DECL( ast::class_variable_declaration_statement )
            RILL_TV_OP_DECL( ast::test_while_statement )
            RILL_TV_OP_DECL( ast::test_if_statement )

            // expression            
            RILL_TV_OP_DECL( ast::element_selector_expression )
            RILL_TV_OP_DECL( ast::call_expression )
            //RILL_TV_OP_DECL( ast::intrinsic_function_call_expression )
            RILL_TV_OP_DECL( ast::binary_operator_expression )
            RILL_TV_OP_DECL( ast::term_expression )

            // value
            RILL_TV_OP_DECL( ast::nested_identifier_value )
            RILL_TV_OP_DECL( ast::identifier_value )
            //RILL_TV_OP_DECL( ast::template_instance_value )
            RILL_TV_OP_DECL( ast::literal_value )

            RILL_TV_OP_FAIL

        private:
            environment_base_ptr root_env_;
            std::shared_ptr<compile_time::llvm_engine const> engine_;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP*/
