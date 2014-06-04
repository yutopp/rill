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
                const_environment_base_ptr const& root_env,
                std::shared_ptr<code_generator::llvm_ir_generator> const& generator,
                std::shared_ptr<llvm::ExecutionEngine> const& execution_engine,
                std::shared_ptr<semantic_analysis::type_detail_pool_t> const& type_detail_pool
                )
                : root_env_( root_env )
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
                std::cout << "ababa => " << arguments.size() << std::endl;
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
                llvm::Function const* const target_function
                ) -> void*
            {
                auto const& function_type = target_function->getFunctionType();

                switch( function_type->getReturnType()->getTypeID() ) {
//                case llvm::Type::VoidTyID:
//                    break;
//
//                case llvm::Type::HalfTyID:
//                    break;
//
//                case llvm::Type::FloatTyID:
//                    break;
//
//                case llvm::Type::DoubleTyID:
//                    break;
//
//                case llvm::Type::X86_FP80TyID:
//                    break;
//
//                case llvm::Type::FP128TyID:
//                    break;
//
//                case llvm::Type::PPC_FP128TyID:
//                    break;
//
//                case llvm::Type::LabelTyID:
//                    break;
//
//                case llvm::Type::MetadataTyID:
//                    break;
//
//                case llvm::Type::X86_MMXTyID:
//                    break;
//
//                case llvm::Type::IntegerTyID:
//                    break;
//
//                case llvm::Type::FunctionTyID:
//                    break;
//
//                case llvm::Type::StructTyID:
//                    break;
//
//                case llvm::Type::ArrayTyID:
//                    break;
//
                case llvm::Type::PointerTyID:
                    return gv.PointerVal;
//
//                case llvm::Type::VectorTyID:
//                    break;
                default:
                    assert( false && "[ice] type" );
                    return nullptr;
                }
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
                std::cout << "CALL expr" << std::endl;

                // ========================================
                // look up self function
                auto const f_env
                    = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
                assert( f_env != nullptr );


                // ========================================
                std::cout << "current : " << f_env->get_mangled_name() << std::endl;
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

                callee_function->dump();

                map_intrinsic_function( callee_function );

                // invocation
                auto const& raw_result
                    = execution_engine_->runFunction( callee_function, total_args );

                return normalize_generic_value( raw_result, callee_function );
            }




            RILL_VISITOR_READONLY_OP( ir_executor, ast::binary_operator_expression, e, parent_env )
            {
                // Look up Function
                auto const f_env
                    = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
                assert( f_env != nullptr );

                std::cout << "current : " << f_env->get_mangled_name() << std::endl;
                llvm::Function* const callee_function
                    = static_cast<llvm::Function*>(
                        ir_generator_->function_env_to_llvm_constatnt_ptr( f_env )
                        );
                if ( !callee_function ) {
                    // unexpected error...
                    assert( false && "unexpected... callee_function was not found" );
                }

                //
                callee_function->dump();

                // call function that defined in rill modules
                // evaluate argument from last to front(but ordering of vector is from front to last)
                ast::expression_list const& e_arguments = { e->lhs_, e->rhs_ };
                auto args = eval_args( e_arguments, parent_env, callee_function );

                auto raw_result = execution_engine_->runFunction( callee_function, args );

                std::cout << "ANs: "
                          << *(raw_result.IntVal.getRawData()) << std::endl;


                assert(false);
                return nullptr;
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::type_expression, e, parent_env )
            {
                return dispatch( e->type_, parent_env );
            }


            RILL_VISITOR_READONLY_OP( ir_executor, ast::term_expression, e, parent_env )
            {
                return dispatch( e->value_, parent_env );
            }


            // identifier node returns Variable
            RILL_VISITOR_READONLY_OP( ir_executor, ast::identifier_value, v, parent_env )
            {
                //
                std::cout << "ir sym solving: "
                          << v->get_inner_symbol()->to_native_string() << std::endl
                          << "ast ptr: " << v.get() << std::endl
                          << (const_environment_base_ptr)parent_env << std::endl;

                //
                //
                auto const& id_env = root_env_->get_related_env_by_ast_ptr( v );
                if ( id_env == nullptr ) {
                    std::cout << "skipped" << std::endl;
                    return nullptr;
                }


                switch( id_env->get_symbol_kind() )
                {
                case kind::type_value::e_variable:
                {
                    std::cout << "llvm_ir_generator -> case Variable!" << std::endl;
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
                        = c_env->make_type_id_from();

                    RILL_DEBUG_LOG( "in llvm.class_name " << c_env->get_qualified_name() << " (" << type_id << ")" );

                    return type_detail_pool_->construct(
                        type_id,
                        nullptr //variable_env
                        );
                }

                default:
                    std::cout << "skipped " << debug_string( id_env->get_symbol_kind() ) << std::endl;
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
