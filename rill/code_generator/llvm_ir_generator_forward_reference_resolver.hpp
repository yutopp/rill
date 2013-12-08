//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_FORWARD_REFERENCE_RESOLVER_HPP
#define RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_FORWARD_REFERENCE_RESOLVER_HPP

#include <memory>
#include <unordered_map>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#include "../ast/detail/tree_visitor_base.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"

#include "llvm_ir_generator_context.hpp"


namespace rill
{
    namespace code_generator
    {
        class llvm_ir_generator_forward_reference_resolver RILL_CXX11_FINAL
            : public ast::detail::tree_visitor_base<llvm::Value*>
        {
        public:
            llvm_ir_generator_forward_reference_resolver(
                const_environment_base_ptr const&,
                intrinsic_function_action_holder_ptr const&,
                llvm_ir_generator_context_ptr const&
                );

        public:
            // statement_list
            RILL_TV_OP_DECL_CONST( ast::root )

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP_DECL_CONST( ast::expression_statement )
            RILL_TV_OP_DECL_CONST( ast::return_statement )
            RILL_TV_OP_DECL_CONST( ast::function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::class_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::variable_declaration_statement )
            RILL_TV_OP_DECL_CONST( ast::class_variable_declaration_statement )
            RILL_TV_OP_DECL_CONST( ast::intrinsic_function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::extern_function_declaration_statement )

            RILL_TV_OP_DECL_CONST( ast::test_while_statement )

           

            // expression
            RILL_TV_OP_DECL_CONST( ast::binary_operator_expression )
            RILL_TV_OP_DECL_CONST( ast::call_expression )
            RILL_TV_OP_DECL_CONST( ast::intrinsic_function_call_expression )
            RILL_TV_OP_DECL_CONST( ast::term_expression )

            //
            RILL_TV_OP_DECL_CONST( ast::intrinsic_value )
            RILL_TV_OP_DECL_CONST( ast::variable_value )

            // TEST
            void debug() const
            {
                context_->llvm_module.dump();
            }

        private:
            const_environment_base_ptr root_env_;
            intrinsic_function_action_holder_ptr action_holder_;

            llvm_ir_generator_context_ptr context_;
        };

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_FORWARD_REFERENCE_RESOLVER_HPP*/
