//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine/ir_executor.hpp>
#include <rill/compile_time/llvm_engine/value_storage.hpp>
#include <rill/compile_time/llvm_engine/value_converter.hpp>
#include <rill/compile_time/llvm_engine/engine_value_holder.hpp>
#include <rill/compile_time/llvm_engine/bridge.hpp>

#include <rill/environment/environment.hpp>
#include <rill/ast/ast.hpp>


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {

            auto is_type_id_value( llvm::Value const* const v )
                -> bool
            {
                return ( reinterpret_cast<std::uintptr_t>( v ) & 0x1 ) != 0;
            }

            auto is_pure_value( llvm::Value const* const v )
                -> bool
            {
                return !( is_type_id_value( v ) );
            }

            auto to_pure_value( llvm::Value* v )
                -> llvm::Value*
            {
                // mask 2bit from LSB.
                return reinterpret_cast<llvm::Value*>(
                    reinterpret_cast<std::uintptr_t>( v ) & ~0x3/*0b11*/
                    );
            }



            ir_executor::ir_executor(
                global_environment_ptr const& g_env,
                std::shared_ptr<code_generator::llvm_ir_generator> const& generator,
                std::shared_ptr<llvm::ExecutionEngine> const& execution_engine,
                std::shared_ptr<semantic_analysis::type_detail_pool_t> const& type_detail_pool
                )
                : g_env_( g_env )
                , ir_generator_( generator )
                , execution_engine_( execution_engine )
                , value_holder_( std::make_shared<engine_value_holder>() )
                , type_detail_pool_( type_detail_pool )
            {}


            auto ir_executor::make_storage( std::size_t const& size, std::size_t const& align ) const
                -> void*
            {
                auto storage = make_dynamic_storage( size, align );
                value_holder_->bind_as_temporary( storage );

                return storage.get();
            }

            auto ir_executor::eval_args(
                ast::expression_list const& arguments,
                const_environment_base_ptr const& parent_env,
                llvm::Function const* const target_function
                ) -> std::vector<llvm::GenericValue>
            {
                rill_dout << "ababa => " << arguments.size() << std::endl;
                std::vector<llvm::GenericValue> gvs;
                auto const& function_type = target_function->getFunctionType();
                std::size_t i = 0;

                for( auto&& argument : arguments ) {
                    auto const& evaled_data = dispatch( argument, parent_env );
                    gvs.push_back(
                        convert_storage_to_generic_value(
                            evaled_data,
                            function_type->getParamType( i )
                            )
                        );

                    ++i;
                }

                return gvs;
            }


            auto ir_executor::normalize_generic_value(
                llvm::GenericValue const& gv,
                type_id_t const& semantic_type_id,
                llvm::Function const* const target_function
                ) -> void*
            {
                auto const& ty = g_env_->get_type_at( semantic_type_id );
                auto const& c_env = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>( ty.class_env_id );

                auto const& function_type = target_function->getFunctionType();
                auto const llvm_type_id = function_type->getReturnType()->getTypeID();

                switch( c_env->get_builtin_kind() ) {
                case class_builtin_kind::k_type:
                {
                    assert( llvm_type_id == llvm::Type::PointerTyID );
                    return gv.PointerVal;
                }

                case class_builtin_kind::k_bool:
                {
                    assert( llvm_type_id == llvm::Type::IntegerTyID );
                    return make_object<bool>( *gv.IntVal.getRawData() );
                }

                case class_builtin_kind::k_int32:
                {
                    assert( llvm_type_id == llvm::Type::IntegerTyID );
                    return make_object<std::int32_t>( *gv.IntVal.getRawData() );
                }

                default:
                {
                    rill_dout << c_env->get_base_name() << std::endl;
                    assert( false && "[[ice, JIT]] this value type was not supported currently." );
                    return nullptr;
                }
                } // switch
            }


            auto ir_executor::map_intrinsic_function(
                llvm::Function const* const target_function
                ) -> void
            {
                std::string const& name = target_function->getName();

                auto const& it = ctfe_intrinsic_function_table.find( name );
                if ( it != ctfe_intrinsic_function_table.cend() ) {
                    if ( mapped_intrinsic_function_names_.find( name ) == mapped_intrinsic_function_names_.cend() ) {
                        // map function manually
                        execution_engine_->addGlobalMapping(
                            target_function,
                            it->second
                            );

                        mapped_intrinsic_function_names_.insert( name );
                    }
                }
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::call_expression, e, parent_env )
            {
                rill_dout << "CALL expr" << std::endl;

                // ========================================
                // look up self function
                auto const f_env
                    = std::static_pointer_cast<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( e ) );
                assert( f_env != nullptr );


                // ========================================
                rill_dout << "current : " << f_env->get_mangled_name() << std::endl;
                llvm::Function* const callee_function
                    = static_cast<llvm::Function*>(
                        ir_generator_->function_env_to_llvm_constatnt_ptr( f_env )
                        );


                auto const& total_args = [&]() -> std::vector<llvm::GenericValue> {
                    if ( f_env->is_in_class() ) {
                        // TODO: see llvm_ir_generator...
                        assert( false && "[ice] not supported" );

                    } else {
                        return eval_args( e->arguments_, parent_env, callee_function );
                    }
                }();

                // evaluate lhs(reciever)
                // if reciever is exist, valid value and type will be stacked
                dispatch( e->reciever_, parent_env );

                // save the reciever object to the temprary space
                if ( f_env->is_in_class() ) {
                    assert( false );
                }

                rill_dregion {
                    callee_function->dump();
                }

                map_intrinsic_function( callee_function );

                // invocation
                auto const& raw_result
                    = execution_engine_->runFunction( callee_function, total_args );

                return normalize_generic_value(
                    raw_result,
                    f_env->get_return_type_id(),
                    callee_function
                    );
            }




            RILL_VISITOR_READONLY_OP( ir_executor, ast::binary_operator_expression, e, parent_env )
            {
                // Look up Function
                auto const f_env
                    = cast_to<function_symbol_environment const>( g_env_->get_related_env_by_ast_ptr( e ) );

                rill_dout << "current : " << f_env->get_mangled_name() << std::endl;

                if ( f_env->has_attribute( attribute::decl::k_intrinsic ) ) {
                    // define llvm function(adhoc)
                    // TODO: cache by f_env->get_action_id()
                    ir_generator_->define_intrinsic_function_frame( f_env );
                }

                llvm::Function* const callee_function
                    = static_cast<llvm::Function*>(
                        ir_generator_->function_env_to_llvm_constatnt_ptr( f_env )
                        );
                if ( !callee_function ) {
                    // unexpected error...
                    assert( false && "unexpected... callee_function was not found" );
                }

                //
                rill_dregion {
                    rill_dout << "callee function" << std::endl;
                    callee_function->dump();
                }

                // call function that defined in rill modules
                // evaluate argument from last to front(but ordering of vector is from front to last)
                ast::expression_list const& e_arguments = { e->lhs_, e->rhs_ };
                auto args = eval_args( e_arguments, parent_env, callee_function );

                auto raw_result = execution_engine_->runFunction( callee_function, args );

                return normalize_generic_value(
                    raw_result,
                    f_env->get_return_type_id(),
                    callee_function
                    );
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::id_expression, e, parent_env )
            {
                return dispatch( e->expression_, parent_env );
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::term_expression, e, parent_env )
            {
                return dispatch( e->value_, parent_env );
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::evaluated_type_expression, e, parent_env )
            {
                return type_detail_pool_->construct(
                    e->type_id,
                    nullptr //variable_env
                    );
            }


            // identifier node returns Variable
            RILL_VISITOR_READONLY_OP( ir_executor, ast::identifier_value, v, parent_env )
            {
                //
                rill_dout << "ir sym solving: "
                          << v->get_inner_symbol()->to_native_string() << std::endl
                          << "ast ptr: " << v.get() << std::endl
                          << (const_environment_base_ptr)parent_env << std::endl;

                //
                //
                auto const& id_env = g_env_->get_related_env_by_ast_ptr( v );
                if ( id_env == nullptr ) {
                    rill_dout << "skipped" << std::endl;
                    return nullptr;
                }


                switch( id_env->get_symbol_kind() )
                {
                case kind::type_value::e_variable:
                {
                    rill_dout << "llvm_ir_generator -> case Variable!" << std::endl;
                    auto const& v_env
                        = std::static_pointer_cast<variable_symbol_environment const>( id_env );
                    assert( v_env != nullptr );

                    // TODO: check the type of variable !
                    // if type is "type", ...(should return id of type...?)

                    // reference the holder of variable...
                    if ( value_holder_->is_defined( v_env->get_id() ) ) {
                        return value_holder_->ref_value( v_env->get_id() );

                    } else {
                        assert( false && "[[ice]] llvm-jit -> value was not found..." );
                    }
                }

                case kind::type_value::e_class:
                {
                    auto const& c_env
                        = std::static_pointer_cast<class_symbol_environment const>( id_env );
                    assert( c_env != nullptr );

                    auto const& type_id
                        = g_env_->make_type_id(
                            c_env->get_id(),
                            attribute::make_value_default()
                            );

                    rill_dregion {
                        std::cout << "in llvm.class_name "
                                  << c_env->get_qualified_name() << " (" << type_id << ")"
                                  << std::endl;
                    }

                    return type_detail_pool_->construct(
                        type_id,
                        nullptr //variable_env
                        );
                }

                default:
                    rill_dout << "skipped " << debug_string( id_env->get_symbol_kind() ) << std::endl;
                    assert( false && "" );
                    return nullptr;
                }
            }


            // identifier node returns Variable
            RILL_VISITOR_READONLY_OP( ir_executor, ast::template_instance_value, v, parent_env )
            {
                //
                rill_dout << "ir sym solving: "
                          << v->get_inner_symbol()->to_native_string() << std::endl
                          << "ast ptr: " << v.get() << std::endl
                          << (const_environment_base_ptr)parent_env << std::endl;

                //
                //
                auto const& id_env = g_env_->get_related_env_by_ast_ptr( v );
                if ( id_env == nullptr ) {
                    rill_dout << "skipped" << std::endl;
                    return nullptr;
                }


                switch( id_env->get_symbol_kind() )
                {
                case kind::type_value::e_variable:
                {
                    rill_dout << "llvm_ir_generator -> case Variable!" << std::endl;
                    auto const& v_env
                        = std::static_pointer_cast<variable_symbol_environment const>( id_env );
                    assert( v_env != nullptr );

                    // TODO: check the type of variable !
                    // if type is "type", ...(should return id of type...?)

                    // reference the holder of variable...
                    if ( value_holder_->is_defined( v_env->get_id() ) ) {
                        return value_holder_->ref_value( v_env->get_id() );

                    } else {
                        assert( false && "[[ice]] llvm-jit -> value was not found..." );
                    }
                }

                case kind::type_value::e_class:
                {
                    auto const& c_env
                        = std::static_pointer_cast<class_symbol_environment const>( id_env );
                    assert( c_env != nullptr );

                    auto const& type_id
                        = g_env_->make_type_id(
                            c_env->get_id(),
                            attribute::make_value_default()
                            );

                    rill_dregion {
                        std::cout << "in llvm.class_name "
                                  << c_env->get_qualified_name() << " (" << type_id << ")"
                                  << std::endl;
                    }

                    return type_detail_pool_->construct(
                        type_id,
                        nullptr //variable_env
                        );
                }

                default:
                    rill_dout << "skipped " << debug_string( id_env->get_symbol_kind() ) << std::endl;
                    assert( false && "" );
                    return nullptr;
                }
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::intrinsic::int32_value, v, parent_env )
            {
                // Currently, return int type( 32bit, integer )
                return make_object<std::int32_t>( v->get_value() );
            }

            RILL_VISITOR_READONLY_OP( ir_executor, ast::intrinsic::boolean_value, v, parent_env )
            {
                return make_object<bool>( v->get_value() );
            }

            RILL_VISITOR_READONLY_OP( ir_executor, ast::intrinsic::string_value, v, parent_env )
            {
                assert( false && "nn" ); // context_->ir_builder.CreateGlobalStringPtr( v->get_value().c_str() );
                return nullptr;
            }

            RILL_VISITOR_READONLY_OP( ir_executor, ast::intrinsic::array_value, v, parent_env )
            {
                assert( false && "not supported" );
                return nullptr;
            }

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill
