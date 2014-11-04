//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/behavior/default_generator.hpp>
#include <rill/behavior/intrinsic_action_holder.hpp>

#include <rill/environment/environment.hpp>
#include <rill/ast/ast.hpp>


namespace rill
{
    namespace behavior
    {
        template<typename Action, typename ActionHolder>
        auto inline register_to_holder(
            ActionHolder& action_holder,
            std::string const& tag_name
            )
            -> void
        {
            action_holder->template append<Action>( tag_name );
        }

        // It defined a function that contains native machine code.
        void register_default_core(
            std::shared_ptr<root_environment> const& root_env,
            std::shared_ptr<intrinsic_action_holder> const& intrinsic_action
            )
        {
            // ============================================================
            // ============================================================
            // Types
            // ============================================================
            {
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_type );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // bind [ type -> i8*(pointer to type_detail) ]
                        context->env_conversion_table.bind_type(
                            c_env,
                            llvm::Type::getInt8Ty( context->llvm_context )->getPointerTo()
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_type"
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_int8 );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // bind [ int8 -> i8 ]
                        context->env_conversion_table.bind_type(
                            c_env,
                            llvm::Type::getInt8Ty( context->llvm_context )
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_int8"
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_int32 );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // bind [ int -> i32 ]
                        context->env_conversion_table.bind_type(
                            c_env,
                            llvm::Type::getInt32Ty( context->llvm_context )
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_int32"
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_void );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // bind [ void -> void ]
                        context->env_conversion_table.bind_type(
                            c_env,
                            llvm::Type::getVoidTy( context->llvm_context )
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_void"
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_bool );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // bind [ bool -> i1 ]
                        context->env_conversion_table.bind_type(
                            c_env,
                            llvm::Type::getInt1Ty( context->llvm_context )
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_bool"
                    );
            }


#if 0
            {
                // template( T: type, N: int )
                // class array

                auto const array_type
                    = rill::ast::make_single_identifier( "array" );

                // template
                rill::ast::parameter_list template_params;

                auto const type_type_expression
                    = rill::ast::helper::make_id_expression( type_class_name );

                // T: type
                rill::ast::variable_declaration ty = {
                    rill::attribute::holder_kind::k_ref,
                    rill::ast::variable_declaration_unit{
                        rill::ast::make_single_identifier( "T" ),
                        rill::ast::value_initializer_unit{
                            type_type_expression,
                            boost::none
                        }
                    }
                };
                template_params.push_back( std::move( ty ) );

                // N: int
                rill::ast::variable_declaration in = {
                    rill::attribute::holder_kind::k_ref,
                    rill::ast::variable_declaration_unit{
                        rill::ast::make_single_identifier( "N" ),
                        rill::ast::value_initializer_unit{
                            rill::ast::helper::make_id_expression(
                                int_class_name
                                ),
                            boost::none
                        }
                    }
                };
                template_params.push_back( std::move( in ) );

                auto const array_class
                    = std::make_shared<rill::ast::class_definition_statement>(
                        array_type
                        );

                auto const template_array
                    = std::make_shared<rill::ast::template_statement>(
                        template_params,
                        array_class
                        );

                id_c.dispatch( template_array, root_env );
            }


            {
                // template( T: type )
                // class ptr

                auto const ptr_type
                    = rill::ast::make_single_identifier( "ptr" );

                // template
                rill::ast::parameter_list template_params;

                auto const type_type_expression
                    = rill::ast::helper::make_id_expression( type_class_name );

                // T: type
                rill::ast::variable_declaration ty = {
                    rill::attribute::holder_kind::k_ref,
                    rill::ast::variable_declaration_unit{
                        rill::ast::make_single_identifier( "T" ),
                        rill::ast::value_initializer_unit{
                            type_type_expression,
                            boost::none
                        }
                    }
                };
                template_params.push_back( std::move( ty ) );

                auto const ptr_class
                    = std::make_shared<rill::ast::class_definition_statement>(
                        ptr_type
                        );

                auto const template_ptr
                    = std::make_shared<rill::ast::template_statement>(
                        template_params,
                        ptr_class
                        );

                id_c.dispatch( template_ptr, root_env );
            }
#endif


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def +( :int, :int ): int => native
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        return context->ir_builder.CreateAdd(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "int_add"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def -( :int, :int ): int => native
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        return context->ir_builder.CreateSub(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "int_sub"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def *( :int, :int ): int => native
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        return context->ir_builder.CreateMul(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "int_mul"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def /( :int, :int ): int => native
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        // Signed div
                        return context->ir_builder.CreateSDiv(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "signed_int_div"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def %( :int, :int ): int => native
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        // Signed remider
                        return context->ir_builder.CreateSRem(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "signed_int_mod"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def <( :int, :int ): bool => native
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        // Signed less than
                        return context->ir_builder.CreateICmpSLT(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "signed_int_less_than"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def ==( val :int, val :int ): bool
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        return context->ir_builder.CreateICmpEQ(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "int_equals"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def =( ref :mutable(int), :int ): ref(mutable(int))
                //
                struct action
                    : rill::intrinsic_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& f_env,
                        std::vector<llvm::Value*> const& argument_vars
                        ) const
                        -> llvm::Value*
                    {
                        assert( argument_vars.size() == 2 );

                        // store
                        return context->ir_builder.CreateStore(
                            argument_vars[1],
                            argument_vars[0]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "int_assign"
                    );
            }

        } // register_default_core

    } // namespace behavior
} // namespace rill
