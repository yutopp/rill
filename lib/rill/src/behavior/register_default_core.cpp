//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/behavior/default_generator.hpp>

#include <rill/environment/environment.hpp>
#include <rill/behavior/intrinsic_function_holder.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace behavior
    {
        template<typename Action, typename ActionHolder, typename RootEnv, typename Identifier, typename ParameterConstructor, typename Env>
        void inline construct_predefined_function(
            ActionHolder& action_holder,
            RootEnv& root_env,
            Identifier const& function_name,
            ParameterConstructor const& tpc_func,
            Env const& return_class_env,
            attribute::type_attributes const& return_type_attr = attribute::make_default_type_attributes()
            )
        {
            // allocate the new action holder
            auto const action_id = action_holder->template append<Action>();

            // function body
            auto const& intrinsic_call_expr = std::make_shared<rill::ast::intrinsic_function_call_expression>( action_id );
            rill::ast::statement_list const sl = { std::make_shared<rill::ast::return_statement>( intrinsic_call_expr ) };
            auto f_ast = std::make_shared<rill::ast::intrinsic_function_definition_statement>( sl );

            // function definition
            auto f = root_env->construct( rill::kind::k_function, function_name, f_ast, tpc_func, return_class_env, return_type_attr );

            // memoize called function env
            f->connect_from_ast( intrinsic_call_expr );
        }
        // It defined a function that contains native machine code.
        // machine code of function entry(brigde of parameters...) and function exit(cleanup) will be generated normally, but
        // function body will be replaced this machine code.




        //
        //
        //
        void register_default_core(
            std::shared_ptr<root_environment> const& root_env,
            std::shared_ptr<intrinsic_function_action_holder> const& intrinsic_function_action
            )
        {
            auto const int_type
                = rill::ast::intrinsic::make_single_identifier( "int" );
            root_env->pre_construct( rill::kind::class_k, int_type );
            auto const int_class_env_pointer = root_env->construct( rill::kind::class_k, int_type );

            auto const string_type
                = rill::ast::intrinsic::make_single_identifier( "string" );
            root_env->pre_construct( rill::kind::class_k, string_type );
            auto const string_class_env_pointer = root_env->construct( rill::kind::class_k, string_type );


            auto const void_type
                = rill::ast::intrinsic::make_single_identifier( "void" );
            root_env->pre_construct( rill::kind::class_k, void_type );
            auto const void_class_env_pointer = root_env->construct( rill::kind::class_k, void_type );

            auto const bool_type
                = rill::ast::intrinsic::make_single_identifier( "bool" );
            root_env->pre_construct( rill::kind::class_k, bool_type );
            auto const bool_class_env_pointer = root_env->construct( rill::kind::class_k, bool_type );


            {
                struct initializer_action
                    : rill::intrinsic_function_action_base
                {
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            // bind [ int -> i32 ]
                            context->env_conversion_table.bind_type(
                                root_env->lookup( ast::intrinsic::make_single_identifier( "int" ) )->get_id(),
                                llvm::Type::getInt32Ty( context->llvm_context )
                                );

                            // bind [ string -> i8* ]
                            context->env_conversion_table.bind_type(
                                root_env->lookup( ast::intrinsic::make_single_identifier( "string" ) )->get_id(),
                                llvm::Type::getInt8Ty( context->llvm_context )->getPointerTo()
                                );

                            // bind [ void -> void ]
                            context->env_conversion_table.bind_type(
                                root_env->lookup( ast::intrinsic::make_single_identifier( "void" ) )->get_id(),
                                llvm::Type::getVoidTy( context->llvm_context )
                                );

                            // bind [ bool -> bool ]
                            context->env_conversion_table.bind_type(
                                root_env->lookup( ast::intrinsic::make_single_identifier( "bool" ) )->get_id(),
                                llvm::Type::getInt1Ty( context->llvm_context )
                                );

                            return nullptr;
                        }
                };

                // set action as initialiser
                intrinsic_function_action->set_initialize_action( std::make_shared<initializer_action>() );
            }












            // TODO: add core.lang namespace


            auto const operator_add
                = rill::ast::intrinsic::make_binary_operator_identifier( "+" );
            auto const operator_multiply
                = rill::ast::intrinsic::make_binary_operator_identifier( "*" );
            auto const operator_sub
                = rill::ast::intrinsic::make_binary_operator_identifier( "-" );
            auto const operator_div
                = rill::ast::intrinsic::make_binary_operator_identifier( "/" );



            //
            auto const operator_less_than
                = rill::ast::intrinsic::make_binary_operator_identifier( "<" );
            auto const operator_assign
                = rill::ast::intrinsic::make_binary_operator_identifier( "=" );


            auto const print
                = rill::ast::intrinsic::make_single_identifier( "print" );




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
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            return context->ir_builder.CreateAdd(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_add, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int

                        return fenv;
                    }, int_class_env_pointer );
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
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            return context->ir_builder.CreateSub(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_sub, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int

                        return fenv;
                    }, int_class_env_pointer );
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
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            return context->ir_builder.CreateMul(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_multiply, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int

                        return fenv;
                    }, int_class_env_pointer );
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
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            // Signed div
                            return context->ir_builder.CreateSDiv(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_div, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int

                        return fenv;
                    }, int_class_env_pointer );
            }











            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def <( :int, :int ): int => native
                //

                struct action
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            // Signed less than
                            return context->ir_builder.CreateICmpSLT(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_less_than, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int

                        return fenv;
                    }, bool_class_env_pointer );
            }












            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def =( ref :int mutable, :int ): int// ref mutable
                //

                struct action
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            // Signed less than
                            context->ir_builder.CreateStore(
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] )
                                );

                            return context->ir_builder.CreateRet(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_assign, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct(
                            nullptr,
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::quality_kind::k_ref,
                                attribute::modifiability_kind::k_mutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct( nullptr, int_class_env_pointer );    // :int

                        return fenv;
                    },
                    int_class_env_pointer,
                    attribute::make_type_attributes(
                        attribute::quality_kind::k_ref,
                        attribute::modifiability_kind::k_mutable
                        )
                    );
            }











            // ============================================================
            // ============================================================
            //
            //
            // ============================================================
            {
                //
                // def print( ref :string ): void => native
                //

                struct action
                    : rill::intrinsic_function_action_base
                {
                    //
                    auto invoke(
                        rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                        code_generator::llvm_ir_generator_context_ptr const& context,
                        const_environment_base_ptr const& root_env,
                        environment_id_list_t const& argument_var_env_ids
                        ) const
                        -> llvm::Value*
                        {
                            // define paramter and return types
                            // TODO: use defined type form env_conversion_table
                            std::vector<llvm::Type*> const parmeter_types
                                = { llvm::Type::getInt8Ty( context->llvm_context )->getPointerTo() };
                            auto const& return_type = llvm::Type::getVoidTy( context->llvm_context );

                            // get function type
                            llvm::FunctionType* const func_type
                                = llvm::FunctionType::get( return_type, parmeter_types, false/*is not variadic*/ );

                            //
                            llvm::Constant* const puts_func
                                = context->llvm_module.getOrInsertFunction( "put_string", func_type );
                            context->ir_builder.CreateCall( puts_func, context->env_conversion_table.ref_value( argument_var_env_ids[0] ) );
                            return context->ir_builder.CreateRetVoid();

                            // return builder->CreateCall( puts_func, llvm_table->ref_value( parameter_variable_decl_env_ids[0] ) );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, print, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( ref :string )
                        fenv->parameter_variable_construct( nullptr, string_class_env_pointer,attribute::make_type_attributes(
                                                                attribute::quality_kind::k_ref) );    // :string

                        return fenv;
                    }, void_class_env_pointer );
            }

        }

    } // namespace behavior
} // namespace rill
