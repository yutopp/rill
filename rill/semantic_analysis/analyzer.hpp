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
#include "../environment/environment_base.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"
#include "../code_generator/llvm_ir_generator_fwd.hpp"
#include "../compile_time/ctfe_engine.hpp"

#include "type_detail.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        class analyzer RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<analyzer, type_detail_ptr>
        {
        public:
            analyzer(
                environment_base_ptr const&,
                intrinsic_function_action_holder_ptr const&
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
            RILL_TV_OP_DECL( ast::type_expression )
            RILL_TV_OP_DECL( ast::term_expression )

            // value
            RILL_TV_OP_DECL( ast::nested_identifier_value )
            RILL_TV_OP_DECL( ast::identifier_value )
            RILL_TV_OP_DECL( ast::template_instance_value )
            RILL_TV_OP_DECL( ast::literal_value )

            RILL_TV_OP_FAIL

            //
            // friends
            //
            friend class code_generator::llvm_ir_generator;

            template<typename AnalyzerPtr, typename EnvPtr>
            friend auto solve_identifier(
                AnalyzerPtr const&,
                ast::const_identifier_value_ptr const&,
                EnvPtr const&,
                bool const = false
                ) -> type_detail_ptr;

            template<typename AnalyzerPtr, typename EnvPtr>
            friend auto solve_identifier(
                AnalyzerPtr const&,
                ast::const_template_instance_value_ptr const&,
                EnvPtr const&,
                bool const = false
                ) -> type_detail_ptr;

            template<typename Visitor,
                     typename TemplateArgs,
                     typename TypeIds,
                     typename EnvPtr,
                     typename ResultCallbackT
                     >
            friend auto overload_solver_with_template(
                Visitor visitor,
                TemplateArgs const& template_args,
                TypeIds const& arg_type_ids2,
                std::shared_ptr<template_set_environment> const& template_set_env,
                EnvPtr const& env,
                ResultCallbackT const& f
                ) -> function_symbol_environment_ptr;

            template<typename AnalyzerPtr, typename EnvPtr>
            friend auto eval_expression_as_ctfe(
                AnalyzerPtr const& a,
                ast::expression_ptr const& expression,
                EnvPtr const& parent_env
                ) -> std::tuple<
                    const_class_symbol_environment_ptr,
                    void*
                >;

        private:
            template<typename AstPtr>
            auto bind_type(
                AstPtr const& ast,
                type_detail_ptr const& ty_p
                ) -> type_detail_ptr
            {
                root_env_->bind_type_id_with_ast( ast, ty_p->type_id );
                return ty_p;
            }


        private:
            environment_base_ptr root_env_;

            std::shared_ptr<type_detail_pool_t> type_detail_pool_;
            std::shared_ptr<compile_time::ctfe_engine const> ctfe_engine_;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP*/
