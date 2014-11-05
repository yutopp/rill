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
                const_environment_base_ptr const&,
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
            RILL_VISITOR_READONLY_OP_DECL( ast::statements );
            RILL_VISITOR_READONLY_OP_DECL( ast::block_statement );
         // RILL_VISITOR_READONLY_OP_DECL( ast::template_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::expression_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::return_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::test_while_statement );
            RILL_VISITOR_READONLY_OP_DECL( ast::test_if_statement );
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
            RILL_VISITOR_READONLY_OP_DECL( ast::id_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::term_expression );
            RILL_VISITOR_READONLY_OP_DECL( ast::evaluated_type_expression );

            // value
            RILL_VISITOR_READONLY_OP_DECL( ast::identifier_value );
            RILL_VISITOR_READONLY_OP_DECL( ast::template_instance_value );

            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::int32_value );
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

            template<typename Px, typename EnvPtr>
            auto eval_args(
                Px const& parameter_type_ids,
                ast::expression_list const& arguments,
                EnvPtr const& parent_env
                ) -> std::vector<llvm::Value*>;

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

            auto type_id_to_llvm_type_ptr( type_id_t const& type_id )
                -> llvm::Type*;

            auto define_intrinsic_function_frame(
                const_function_symbol_environment_ptr const& f_env
                )
                -> void;

        private:
            const_environment_base_ptr root_env_;
            intrinsic_action_holder_ptr action_holder_;

            llvm_ir_generator_context_ptr context_;
            semantic_analysis::analyzer* analyzer_;
        };

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/
