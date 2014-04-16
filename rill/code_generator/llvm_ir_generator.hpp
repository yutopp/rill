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

#include "../ast/detail/tree_visitor_base.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"
#include "../semantic_analysis/analyzer_fwd.hpp"

#include "llvm_ir_generator_fwd.hpp"
#include "llvm_ir_generator_context.hpp"


namespace rill
{
    namespace code_generator
    {
        // ========================================
        class llvm_ir_generator RILL_CXX11_FINAL
            : public ast::detail::const_tree_visitor<llvm_ir_generator, llvm::Value*>
        {
        public:
            llvm_ir_generator(
                const_environment_base_ptr const&,
                intrinsic_function_action_holder_ptr const&,
                llvm_ir_generator_context_ptr const&,
                semantic_analysis::analyzer* const = nullptr
                );

        public:
            friend class type_id_to_llvm_type_ptr;

            auto function_env_to_llvm_constatnt_ptr(
                const_function_symbol_environment_ptr const& f_env
                ) const -> llvm::Constant*;

        public:
            // statement_list
            RILL_TV_OP_DECL_CONST( ast::statements )
            RILL_TV_OP_DECL_CONST( ast::block_statement )
         // RILL_TV_OP_DECL_CONST( ast::template_statement )
            RILL_TV_OP_DECL_CONST( ast::expression_statement )
            RILL_TV_OP_DECL_CONST( ast::return_statement )
            RILL_TV_OP_DECL_CONST( ast::test_while_statement )
            RILL_TV_OP_DECL_CONST( ast::test_if_statement )
            RILL_TV_OP_DECL_CONST( ast::function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::variable_declaration_statement )
            RILL_TV_OP_DECL_CONST( ast::extern_function_declaration_statement )
            RILL_TV_OP_DECL_CONST( ast::intrinsic_function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::class_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::class_function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::class_variable_declaration_statement )

            // expression
            RILL_TV_OP_DECL_CONST( ast::element_selector_expression )
            RILL_TV_OP_DECL_CONST( ast::subscrpting_expression )
            RILL_TV_OP_DECL_CONST( ast::call_expression )
            RILL_TV_OP_DECL_CONST( ast::binary_operator_expression )
            RILL_TV_OP_DECL_CONST( ast::intrinsic_function_call_expression )
            RILL_TV_OP_DECL_CONST( ast::type_expression )
            RILL_TV_OP_DECL_CONST( ast::term_expression )

            // value
            //RILL_TV_OP_DECL_CONST( ast::nested_identifier_value )
            RILL_TV_OP_DECL_CONST( ast::identifier_value )
            RILL_TV_OP_DECL_CONST( ast::template_instance_value )

            RILL_TV_OP_DECL_CONST( ast::intrinsic::int32_value )
            RILL_TV_OP_DECL_CONST( ast::intrinsic::boolean_value )
            RILL_TV_OP_DECL_CONST( ast::intrinsic::string_value )
            RILL_TV_OP_DECL_CONST( ast::intrinsic::array_value )

            RILL_TV_OP_FAIL

        public:
            // TEST
            void debug() const
            {
                context_->llvm_module.dump();
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
                ) const -> llvm::Value*;

            template<typename Px, typename EnvPtr>
            auto eval_args(
                Px const& parameter_type_ids,
                ast::expression_list const& arguments,
                EnvPtr const& parent_env
                ) const -> std::vector<llvm::Value*>;

            template<typename EnvPtr>
            auto store_value(
                type const& arg_type,
                llvm::Type* const variable_llvm_type,
                llvm::Value* const arg_value,
                EnvPtr const& v_env
                ) const -> void;

        private:
            const_environment_base_ptr root_env_;
            intrinsic_function_action_holder_ptr action_holder_;

            llvm_ir_generator_context_ptr context_;
            semantic_analysis::analyzer* analyzer_;
        };

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/
