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
        template<typename Action, typename ActionHolder>
        auto inline construct_predefined_function(
            ActionHolder& action_holder,
            std::string const& tag_name
            )
            -> void
        {
            action_holder->template append<Action>( tag_name );
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
                        std::vector<llvm::Value*> const&
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
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
                    : rill::intrinsic_function_action_base
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
                construct_predefined_function<action>(
                    intrinsic_function_action,
                    "int_assign"
                    );
            }

        } // register_default_core

    } // namespace behavior
} // namespace rill
