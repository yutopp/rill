//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP
#define RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP

#include <memory>
#include <unordered_set>

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include "../ast/detail/tree_visitor_base.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"

#include "llvm_ir_generator_context.hpp"
#include "llvm_ir_generator.hpp"


namespace rill
{
    namespace code_generator
    {
        // ========================================
        class type_id_to_llvm_type_ptr;


        // ========================================
        class llvm_ir_executor RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<llvm_ir_executor, llvm::GenericValue>
        {
        public:
            llvm_ir_generator(
                const_environment_base_ptr const&,
                intrinsic_function_action_holder_ptr const&,
                llvm_ir_generator_context_ptr const&
                );

        public:
            friend class type_id_to_llvm_type_ptr;
            
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
            RILL_TV_OP_DECL_CONST( ast::binary_operator_expression )
            RILL_TV_OP_DECL_CONST( ast::element_selector_expression )
            RILL_TV_OP_DECL_CONST( ast::call_expression )
            RILL_TV_OP_DECL_CONST( ast::intrinsic_function_call_expression )
            RILL_TV_OP_DECL_CONST( ast::term_expression )

            // value
            //RILL_TV_OP_DECL_CONST( ast::nested_identifier_value )
            RILL_TV_OP_DECL_CONST( ast::identifier_value )
            //RILL_TV_OP_DECL_CONST( ast::template_instance_value )
            RILL_TV_OP_DECL_CONST( ast::literal_value )

            RILL_TV_OP_FAIL


            // TEST
            void debug() const
            {
                context_->llvm_module.dump();
            }


            auto is_built( ast::const_ast_base_ptr const& node ) const
                -> bool
            {
                return built_set_.count( node ) != 0;
            }

            void check( ast::const_ast_base_ptr const& node ) const
            {
                built_set_.insert( node );
            }

        private:
            const_environment_base_ptr root_env_;
            intrinsic_function_action_holder_ptr action_holder_;

            llvm_ir_generator_context_ptr context_;
            mutable std::unordered_set<ast::const_ast_base_ptr> built_set_;
        };

#if 0
        template<typename EnvIdT, typename IdTablePtr, typename IRBuilderPtr>
        auto inline ref_value_with( EnvIdT const& env_id, IdTablePtr const& table, IRBuilderPtr builder )
            -> llvm::Value*
        {
            auto const ref_value = table.ref_value( env_id );
            if ( table.is_alloca_inst( env_id ) ) {
                return builder.CreateLoad( ref_value );
            }
            return ref_value;
        }
#endif

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/
