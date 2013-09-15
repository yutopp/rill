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
#include <unordered_map>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#include "../ast/detail/tree_visitor_base.hpp"


namespace rill
{
    namespace code_generator
    {
        class llvm_ir_generator RILL_CXX11_FINAL
            : public ast::detail::tree_visitor_base<llvm::Value*>
        {
        public:
            llvm_ir_generator();

        public:
            // statement_list
            RILL_TV_OP_DECL( ast::root )

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP_DECL( ast::expression_statement )
            //RILL_TV_OP_DECL( ast::return_statement )
            RILL_TV_OP_DECL( ast::function_definition_statement )
            RILL_TV_OP_DECL( ast::embedded_function_definition_statement )

            //RILL_TV_OP_DECL( ast::class_definition_statement )

            // expression
            RILL_TV_OP_DECL( ast::binary_operator_expression )
            //RILL_TV_OP_DECL( ast::call_expression )
            //RILL_TV_OP_DECL( ast::embedded_function_call_expression_ptr )
            RILL_TV_OP_DECL( ast::term_expression )

            //
            RILL_TV_OP_DECL( ast::intrinsic_value )
            //RILL_TV_OP_DECL( ast::variable_value )

            void debug()
            {
                module_->dump();
            }



        private:
            llvm::LLVMContext& context_;
            std::shared_ptr<llvm::Module> module_;
            std::shared_ptr<llvm::IRBuilder<>> builder_;

            class env_id_llvm_table
            {
            public:
                auto bind_value( environment_id_t const& env_id, llvm::Value* const value )
                    -> void
                {
                    // TODO: dup check
                    value_table_.emplace( env_id, value );
                }

                auto ref_value( environment_id_t const& env_id )
                    -> llvm::Value*
                {
                    return value_table_.at( env_id );
                }

                auto bind_type( environment_id_t const& env_id, llvm::Type* const value )
                    -> void
                {
                    // TODO: dup check
                    type_table_.emplace( env_id, value );
                }

                auto ref_type( environment_id_t const& env_id )
                    -> llvm::Type*
                {
                    return type_table_.at( env_id );
                }

            private:
                std::unordered_map<environment_id_t, llvm::Value*> value_table_;
                std::unordered_map<environment_id_t, llvm::Type*> type_table_;
            };

            std::shared_ptr<env_id_llvm_table> llvm_table_;
        };
    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/