//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP
#define RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#include "../ast/visitor.hpp"
#include "../behavior/intrinsic_action_holder_fwd.hpp"
#include "../semantic_analysis/analyzer_fwd.hpp"
#include "../compile_time/llvm_engine/ir_executor_fwd.hpp"
#include "../environment/global_environment.hpp"

#include "llvm_ir_generator_fwd.hpp"
#include "llvm_ir_generator_context.hpp"


namespace rill
{
    namespace code_generator
    {
        // ========================================
        class llvm_ir_generator RILL_CXX11_FINAL
            : public ast::readonly_ast_visitor<llvm_ir_generator, llvm::Value*>
        {
        public:
            using self_type = llvm_ir_generator;
            RILL_VISITOR_OP_DEFAULT

        public:
            llvm_ir_generator(
                const_global_environment_ptr const&,
                intrinsic_action_holder_ptr const&,
                llvm_ir_generator_context_ptr const&,
                semantic_analysis::analyzer* const = nullptr
                );

        public:
            friend class type_id_to_llvm_type_ptr;
            friend class compile_time::llvm_engine::ir_executor;

            auto function_env_to_llvm_constatnt_ptr(
                const_function_symbol_environment_ptr const& f_env
                ) -> llvm::Constant*;

        public:
            // statement_list
            RILL_VISITOR_READONLY_OP_DECL( ast::module );
            RILL_VISITOR_READONLY_OP_DECL( ast::statements );
            RILL_VISITOR_READONLY_OP_DECL( ast::block_statement );
         // RILL_VISITOR_READONLY_OP_DECL( ast::template_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::expression_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::return_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::while_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::if_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::function_definition_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::variable_declaration_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::extern_function_declaration_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::extern_class_declaration_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::class_definition_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::class_function_definition_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::class_variable_declaration_statement );

            // expression
            RILL_VISITOR_READONLY_OP_DECL( ast::element_selector_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::subscrpting_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::call_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::binary_operator_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::unary_operator_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::id_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::dereference_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::addressof_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::lambda_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::term_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::evaluated_type_expression );

            // value
            RILL_VISITOR_READONLY_OP_DECL( ast::captured_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::identifier_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::template_instance_value );

            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::int32_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::float_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::boolean_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::string_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::array_value );

        public:
            // TEST
            void debug() const
            {
                context_->llvm_module->dump();
            }

            inline auto is_jit() const
                -> bool
            {
                return analyzer_ != nullptr;
            }


        private:
            auto convert_value_by_attr(
                type const& target_type,
                type const& source_type,
                llvm::Value* const source_value
                ) -> llvm::Value*;

            template<typename Px>
            auto eval_args(
                Px const& parameter_type_ids,
                ast::expression_list const& arguments,
                const_environment_base_ptr const& parent_env
                )
                -> std::vector<llvm::Value*>
            {
                std::vector<llvm::Value*> args;

                eval_args(
                    parameter_type_ids,
                    std::back_inserter( args ),
                    arguments,
                    parent_env
                    );

                return args;
            }

            template<typename Px, typename It>
            auto eval_args(
                Px const& parameter_type_ids,
                It it,
                ast::expression_list const& arguments,
                const_environment_base_ptr const& parent_env
                )
                -> void
            {
                // evaluate argument front to last
                for( std::size_t i=0; i<arguments.size(); ++i ) {
                    auto const& parameter_type
                        = g_env_->get_type_at( parameter_type_ids[i] );
                    auto const& arg_type_id
                        = g_env_->get_related_type_id_from_ast_ptr(
                            arguments[i]
                            );
                    if ( arg_type_id == type_id_undefined ) {
                        rill_dregion {
                            arguments[i]->dump( std::cout );
                        }
                        assert( false );
                    }
                    auto const arg_type
                        = g_env_->get_type_at( arg_type_id );
                    auto const& arg_value
                        = dispatch( arguments[i], parent_env );
                    assert( arg_value != nullptr );

                    auto result_value = convert_value_by_attr(
                        parameter_type,
                        arg_type,
                        arg_value
                        );

                    it = std::move( result_value );
                }
            }

            auto store_value(
                llvm::Value* const arg_value,
                const_variable_symbol_environment_ptr const& v_env
                )
                -> void;

            auto regard_env_is_defined( environment_id_t const& env_id )
                -> void;

        private:
            auto is_heavy_object( type const& ) const
                -> bool;

            auto is_represented_as_pointer( type const&, llvm::Value* const ) const
                -> bool;

            auto type_id_to_llvm_type_ptr( type_id_t const& type_id )
                -> llvm::Type*;

            auto define_intrinsic_function_frame(
                const_function_symbol_environment_ptr const& f_env
                )
                -> void;

        private:
            auto generate_function_call(
                const_function_symbol_environment_ptr const& f_env,
                ast::expression_list const& e_arguments,
                const_environment_base_ptr const& parent_env,
                bool const is_operator = false
                )
                -> llvm::Value*;

            auto delegate_value_to(
                type const&,
                llvm::Value* const from,
                llvm::Value* const to
                )
                -> void;

            auto get_class_size(
                const_class_symbol_environment_ptr const&
                ) const
                -> std::size_t;

            auto get_class_alignment(
                const_class_symbol_environment_ptr const&
                ) const
                -> std::size_t;

        private:
            const_global_environment_ptr g_env_;
            intrinsic_action_holder_ptr action_holder_;

            llvm_ir_generator_context_ptr context_;
            semantic_analysis::analyzer* analyzer_;
        };

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/
