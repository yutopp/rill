//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/ctfe_engine.hpp>
#include <rill/compile_time/engine_value_holder.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace compile_time
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



        ctfe_engine::ctfe_engine(
            std::shared_ptr<code_generator::llvm_ir_generator const> const& generator,
            std::shared_ptr<llvm::ExecutionEngine> const& execution_engine,
            std::shared_ptr<semantic_analysis::type_detail_pool_t> const& type_detail_pool
            )
            : ir_generator_( generator )
            , execution_engine_( execution_engine )
            , value_holder_( std::make_shared<engine_value_holder>() )
            , type_detail_pool_( type_detail_pool )
        {}


        RILL_TV_OP_CONST( ctfe_engine, ast::binary_operator_expression, e, parent_env )
        {
            llvm::Value* gen_val
                = ir_generator_->dispatch( e, parent_env );

            gen_val->dump();
            assert(false);
            return nullptr;
        }



        RILL_TV_OP_CONST( ctfe_engine, ast::type_expression, e, parent_env )
        {
            return dispatch( e->type_, parent_env );
        }


        RILL_TV_OP_CONST( ctfe_engine, ast::term_expression, e, parent_env )
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

    } // namespace compile_time
} // namespace rill
