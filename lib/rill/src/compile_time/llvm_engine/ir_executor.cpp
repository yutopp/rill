//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine/ir_executor.hpp>
#include <rill/compile_time/llvm_engine/value_storage.hpp>
#include <rill/compile_time/llvm_engine/engine_value_holder.hpp>

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



            RILL_VISITOR_READONLY_OP( ir_executor, ast::binary_operator_expression, e, parent_env )
            {
                // Look up Function
                auto const f_env
                    = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
                assert( f_env != nullptr );

                std::cout << "current : " << f_env->mangled_name() << std::endl;
                llvm::Function* callee_function
                    = static_cast<llvm::Function*>(
                        ir_generator_->function_env_to_llvm_constatnt_ptr( f_env )
                        );
                if ( !callee_function ) {
                    // unexpected error...
                    assert( false && "unexpected... callee_function was not found" );
                }

                //
                callee_function->dump();

                llvm::GenericValue a;
                a.IntVal = llvm::APInt( 32, 72 );

                std::vector<llvm::GenericValue> args = { a, a };
                auto const& raw_result = execution_engine_->runFunction( callee_function, args );

                std::cout << *(a.IntVal.getRawData())
                          << " = " << *(raw_result.IntVal.getRawData()) << std::endl;

                assert(false);
                return nullptr;
            }


            auto convert_value_from_llvm_world_to_rill_world( llvm::Value* v )
                -> void
            {

            }



            RILL_VISITOR_READONLY_OP( ir_executor, ast::type_expression, e, parent_env )
            {
                return dispatch( e->type_, parent_env );
            }


            // identifier node returns Variable
            RILL_VISITOR_READONLY_OP( ir_executor, ast::identifier_value_base, v, parent_env )
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

                std::cout << "in llvm.class_name " << c_env->get_qualified_name() << " (" << type_id << ")" << std::endl;

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
                return make_object<int>( v->get_value() );
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


#if 0
        RILL_VISITOR_READONLY_OP( ir_executor, ast::term_expression, e, parent_env )
        {
            //
            llvm::Value* gen_val
                = ir_generator_->dispatch( e, parent_env );
            assert( gen_val != nullptr );

            llvm::Value* pure_val_ptr
                = to_pure_value( gen_val );

            if ( is_pure_value( gen_val ) ) {
                // pure value
                return pure_val_ptr;

            } else if ( is_type_id_value( gen_val ) ) {
                // Type Id
                llvm::ConstantInt const* const type_id_value_ptr
                    = static_cast<llvm::ConstantInt const* const>( pure_val_ptr );

                type_id_t const type_id
                    = type_id_t( type_id_value_ptr->getZExtValue() );

                std::cout << "tpre!! " << type_id << std::endl;

                return type_detail_pool_->construct(
                    type_id,
                    nullptr //variable_env
                    );
            } else {
                assert( false );
            }

            return nullptr;
        }
#endif

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill
