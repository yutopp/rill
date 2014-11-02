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
#include <rill/semantic_analysis/identifier_collector.hpp>

#include <rill/ast/ast.hpp>


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
            attribute::type_attributes const& return_type_attr
                = attribute::make_type_attributes(
                    attribute::holder_kind::k_val,
                    attribute::modifiability_kind::k_immutable
                    )
            )
        {
            // allocate the new action holder
            auto const action_id = action_holder->template append<Action>();

            // function body
            auto const& intrinsic_call_expr = std::make_shared<rill::ast::intrinsic_function_call_expression>( action_id );
            ast::element::statement_list const sl = {
                std::make_shared<rill::ast::return_statement>( intrinsic_call_expr )
            };
            auto f_ast = std::make_shared<rill::ast::intrinsic_function_definition_statement>(
                function_name,
                std::make_shared<ast::statements>( sl )
                );

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
            auto id_c = semantic_analysis::identifier_collector(
                semantic_analysis::collection_type::e_builtin
                );

//
#define RILL_DEFINE_BUILTIN_CLASS( name, kind )                         \
            auto const name ## _class_name = rill::ast::make_single_identifier( #name ); \
            auto const name ## _class = std::make_shared<rill::ast::class_definition_statement>( name ## _class_name ); \
            id_c.dispatch( name ## _class, root_env );                  \
            auto const name ## _class_env_pointer = std::static_pointer_cast<class_symbol_environment>( \
                root_env->get_related_env_by_ast_ptr( name ## _class )  \
                );                                                      \
            name ## _class_env_pointer->set_builtin_kind( rill::class_builtin_kind:: kind );

            RILL_DEFINE_BUILTIN_CLASS( type, k_type );
            RILL_DEFINE_BUILTIN_CLASS( int8, k_int8 );
            RILL_DEFINE_BUILTIN_CLASS( int, k_int32 );
            RILL_DEFINE_BUILTIN_CLASS( string, k_string );
            RILL_DEFINE_BUILTIN_CLASS( void, k_void );
            RILL_DEFINE_BUILTIN_CLASS( bool, k_bool );
            //RILL_DEFINE_BUILTIN_CLASS( double );

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
                            auto const install_type = [&]( std::string const& type_name, llvm::Type* const ty ) {
                                auto const& lookup_env = root_env->lookup( ast::make_single_identifier( type_name ) );
                                assert( lookup_env != nullptr );
                                assert( lookup_env->get_symbol_kind() == kind::type_value::e_multi_set );
                                auto const& multi_set_env = cast_to<multiple_set_environment>( lookup_env );
                                assert( multi_set_env->get_representation_kind() == kind::type_value::e_class );
                                auto const& class_env = multi_set_env->get_unique_environment<class_symbol_environment>();
                                assert( class_env != nullptr );

                                assert( ty != nullptr );

                                context->env_conversion_table.bind_type( class_env, ty );
                            };


                            // bind [ bool -> i1 ]
                            install_type(
                                "bool",
                                llvm::Type::getInt1Ty( context->llvm_context )
                                );

                            // bind [ int8 -> i8 ]
                            install_type(
                                "int8",
                                llvm::Type::getInt8Ty( context->llvm_context )
                                );

                            // bind [ int -> i32 ]
                            install_type(
                                "int",
                                llvm::Type::getInt32Ty( context->llvm_context )
                                );

                            // bind [ string -> i8* ]
                            install_type(
                                "string",
                                llvm::Type::getInt8Ty( context->llvm_context )->getPointerTo()
                                );

                            // bind [ void -> void ]
                            install_type(
                                "void",
                                llvm::Type::getVoidTy( context->llvm_context )
                                );

                            // bind [ type -> i8*(pointer to type_detail) ]
                            install_type(
                                "type",
                                llvm::Type::getInt8Ty( context->llvm_context )->getPointerTo()
                                );

                            return nullptr;
                        }
                };

                // set action as initialiser
                intrinsic_function_action->set_initialize_action( std::make_shared<initializer_action>() );
            }












            // TODO: add core.lang namespace


            auto const operator_add
                = rill::ast::make_binary_operator_identifier( "+" );
            auto const operator_multiply
                = rill::ast::make_binary_operator_identifier( "*" );
            auto const operator_sub
                = rill::ast::make_binary_operator_identifier( "-" );
            auto const operator_div
                = rill::ast::make_binary_operator_identifier( "/" );
            auto const operator_modulo
                = rill::ast::make_binary_operator_identifier( "%" );


            //
            auto const operator_less_than
                = rill::ast::make_binary_operator_identifier( "<" );
            auto const operator_assign
                = rill::ast::make_binary_operator_identifier( "=" );
            auto const operator_equal
                = rill::ast::make_binary_operator_identifier( "==" );


            auto const print
                = rill::ast::make_single_identifier( "print" );




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
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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
                // def %( :int, :int ): int => native
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
                            // Signed remider
                            return context->ir_builder.CreateSRem(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_modulo, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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

                            return context->env_conversion_table.ref_value( argument_var_env_ids[0] );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_assign, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_ref,
                                attribute::modifiability_kind::k_mutable
                                )
                            );    // ref :mutable!int

                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

                        return fenv;
                    },
                    int_class_env_pointer,
                    attribute::make_type_attributes(
                        attribute::holder_kind::k_ref,
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
                // def ==( :int, :int ): bool => native
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
                            //
                            return context->ir_builder.CreateICmpEQ(
                                context->env_conversion_table.ref_value( argument_var_env_ids[0] ),
                                context->env_conversion_table.ref_value( argument_var_env_ids[1] )
                                );
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, operator_equal, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( :int, :int )
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__b" ),
                            int_class_env_pointer,
                            attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :int

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
                // def print( val :string ): void => native
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
                                = context->llvm_module->getOrInsertFunction( "put_string", func_type );

                            context->ir_builder.CreateCall( puts_func, context->env_conversion_table.ref_value( argument_var_env_ids[0] ) );

                            return 0;  // IRBuilder will recoginize 0 as Void...
                        }
                };

                construct_predefined_function<action>( intrinsic_function_action, root_env, print, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                        // ( ref :string )
                        fenv->parameter_variable_construct(
                            rill::ast::make_single_identifier( "__a" ),
                            string_class_env_pointer,attribute::make_type_attributes(
                                attribute::holder_kind::k_val,
                                attribute::modifiability_kind::k_immutable
                                )
                            );    // :string

                        return fenv;
                    }, void_class_env_pointer );
            }

        }

    } // namespace behavior
} // namespace rill
