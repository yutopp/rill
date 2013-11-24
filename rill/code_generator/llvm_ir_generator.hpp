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
#include "../utility/embedded_function_holder_fwd.hpp"


namespace rill
{
    namespace code_generator
    {
        class llvm_ir_generator RILL_CXX11_FINAL
            : public ast::detail::tree_visitor_base<llvm::Value*>
        {
        public:

            // consists env_id and llvm_object.
            class env_id_llvm_table
            {
            public:
                //
                // Values
                //
                auto bind_value( environment_id_t const& env_id, llvm::Value* const value, bool const is_alloca_inst = false )
                    -> void
                {
                    // TODO: dup check
                    value_table_.emplace( env_id, value );
                    require_load_inst_[env_id] = is_alloca_inst;
                }

                //
                auto bind_value( environment_id_t const& env_id, llvm::AllocaInst* const alloca_inst )
                    -> void
                {
                    bind_value( env_id, static_cast<llvm::Value*>( alloca_inst ), true );
//                    require_load_inst_[env_id] = true;
                }


                auto ref_value( environment_id_t const& env_id ) const
                    -> llvm::Value*
                {
                    return value_table_.at( env_id );
                }

                auto is_alloca_inst( environment_id_t const& env_id ) const
                    -> bool
                {
                    return require_load_inst_.at( env_id );
                }

                //
                // Types
                //
                auto bind_type( environment_id_t const& env_id, llvm::Type* const type )
                    -> void
                {
                    // TODO: dup check
                    type_table_.emplace( env_id, type );
                }

                auto ref_type( environment_id_t const& env_id ) const
                    -> llvm::Type*
                {
                    return type_table_.at( env_id );
                }


                //
                // Functions
                //
                auto bind_function_type( environment_id_t const& env_id, llvm::FunctionType* const f_type )
                    -> void
                {
                    // TODO: dup check
                    function_type_table_.emplace( env_id, f_type );
                }

                auto ref_function_type( environment_id_t const& env_id ) const
                    -> llvm::FunctionType*
                {
                    return function_type_table_.at( env_id );
                }


                //
                //
                //
                auto is_defined( environment_id_t const& env_id ) const
                    -> bool
                {
                    return ( value_table_.find( env_id ) != value_table_.cend() )
                        || ( type_table_.find( env_id ) != type_table_.cend() )
                        || ( function_type_table_.find( env_id ) != function_type_table_.cend() )
                        ;
                }

            private:
                std::unordered_map<environment_id_t, llvm::Value*> value_table_;
                std::unordered_map<environment_id_t, llvm::Type*> type_table_;
                std::unordered_map<environment_id_t, llvm::FunctionType*> function_type_table_;

                std::unordered_map<environment_id_t, bool> require_load_inst_;
            };

        public:
            llvm_ir_generator( const_environment_ptr const&, embedded_function_holder_ptr const& );

        public:
            // statement_list
            RILL_TV_OP_DECL_CONST( ast::root )

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP_DECL_CONST( ast::expression_statement )
            RILL_TV_OP_DECL_CONST( ast::return_statement )
            RILL_TV_OP_DECL_CONST( ast::function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::variable_declaration_statement )
            RILL_TV_OP_DECL_CONST( ast::embedded_function_definition_statement )
            RILL_TV_OP_DECL_CONST( ast::extern_function_declaration_statement )

            //RILL_TV_OP_DECL( ast::class_definition_statement )

            // expression
            RILL_TV_OP_DECL_CONST( ast::binary_operator_expression )
            RILL_TV_OP_DECL_CONST( ast::call_expression )
            RILL_TV_OP_DECL_CONST( ast::embedded_function_call_expression )
            RILL_TV_OP_DECL_CONST( ast::term_expression )

            //
            RILL_TV_OP_DECL_CONST( ast::intrinsic_value )
            RILL_TV_OP_DECL_CONST( ast::variable_value )

            // TEST
            void debug() const
            {
                module_->dump();
            }


            // const???
            auto get_llvm_module() const
                -> std::shared_ptr<llvm::Module>
            {
                return module_;
            }

        private:
            llvm::LLVMContext& context_;
            std::shared_ptr<llvm::Module> module_;
            std::shared_ptr<llvm::IRBuilder<>> builder_;
            std::shared_ptr<env_id_llvm_table> llvm_table_;

            const_environment_ptr root_env_;
            embedded_function_holder_ptr action_holder_;
        };
    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/
