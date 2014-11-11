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
        template<typename Action, typename ActionHolder, typename... Args>
        auto inline register_to_holder(
            ActionHolder& action_holder,
            std::string const& tag_name,
            Args&&... args
            )
            -> void
        {
            action_holder->append(
                tag_name,
                std::make_shared<Action>( std::forward<Args>( args )... )
                );
        }


        // It defined a function that contains native machine code.
        void register_default_core(
            std::shared_ptr<intrinsic_action_holder> const& intrinsic_action,
            std::shared_ptr<total_system_info_setter> const& sis
            )
        {
            using sis_t = std::shared_ptr<total_system_info_setter>;

            // ============================================================
            // ============================================================
            // Types
            // ============================================================
            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

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

                   sis_t sis_;
                };

                register_to_holder<action>(
                    intrinsic_action,
                    "type_type",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_int8 );
                        sis_->set( rill::class_builtin_kind::k_int8, c_env );
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

                    sis_t sis_;
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_int8",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_int32 );
                        sis_->set( rill::class_builtin_kind::k_int32, c_env );
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

                    sis_t sis_;
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_int32",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_float );
                        sis_->set( rill::class_builtin_kind::k_float, c_env );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // bind [ float -> i32 ]
                        context->env_conversion_table.bind_type(
                            c_env,
                            llvm::Type::getFloatTy( context->llvm_context )
                            );
                    }

                    sis_t sis_;
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_float",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

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

                    sis_t sis_;
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_void",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_bool );
                        sis_->set( rill::class_builtin_kind::k_bool, c_env );
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

                    sis_t sis_;
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "type_bool",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_ptr );
                        sis_->set( rill::class_builtin_kind::k_ptr, c_env );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // do nothing
                    }

                   sis_t sis_;
                };

                register_to_holder<action>(
                    intrinsic_action,
                    "type_ptr",
                    sis
                    );
            }

            {
                struct action
                    : rill::intrinsic_action_base
                {
                    action( sis_t const& s )
                        : sis_( s )
                    {}

                    auto invoke(
                        rill::processing_context::semantics_typing_tag,
                        class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        c_env->set_builtin_kind( rill::class_builtin_kind::k_array );
                    }

                    auto invoke(
                        rill::processing_context::llvm_ir_generator_typing_tag,
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_class_symbol_environment_ptr const& c_env
                        ) const
                        -> void
                    {
                        // do nothing
                    }

                   sis_t sis_;
                };

                register_to_holder<action>(
                    intrinsic_action,
                    "type_array",
                    sis
                    );
            }

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

            {
                //
                // def op <=( val :int, val :int ): int
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

                        // Signed less than or equal
                        return context->ir_builder.CreateICmpSLE(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "signed_int_less_than_or_equal"
                    );
            }

            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def =( ref :mutable(T), :int ): ref(mutable(T))
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
                    "store_value_b_to_a"
                    );
            }


            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            // ref: http://llvm.org/docs/LangRef.html#fcmp-instruction
            // Ordered means that neither operand is a QNAN while unordered means that either operand may be a QNAN.
            {
                //
                // def >( val :float, val :float ): float
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

                        // unordered
                        return context->ir_builder.CreateFCmpUGT(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "float_unordered_greater_than"
                    );
            }

            {
                //
                // def op pre -( val :int ): int
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
                        assert( argument_vars.size() == 1 );

                        return context->ir_builder.CreateNeg(
                            argument_vars[0]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "signed_int_negate"
                    );
            }


            {
                //
                // def op pre -( val :float ): float
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
                        assert( argument_vars.size() == 1 );

                        return context->ir_builder.CreateFNeg(
                            argument_vars[0]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "float_negate"
                    );
            }

            {
                //
                // def op <=( val :float, val :float ): float
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

                        // unordered
                        return context->ir_builder.CreateFCmpULE(
                            argument_vars[0],
                            argument_vars[1]
                            );
                    }
                };
                register_to_holder<action>(
                    intrinsic_action,
                    "float_unordered_less_than_or_equal"
                    );
            }

        } // register_default_core

    } // namespace behavior
} // namespace rill
