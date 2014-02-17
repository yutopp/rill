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
        ctfe_engine::ctfe_engine(
            std::shared_ptr<code_generator::llvm_ir_generator> const& generator,
            std::shared_ptr<llvm::ExecutionEngine> const& execution_engine,
            std::shared_ptr<semantic_analysis::type_detail_pool_t> const& type_detail_pool
            )
            : ir_generator_( generator )
            , execution_engine_( execution_engine )
            , value_holder_( std::make_shared<engine_value_holder>() )
            , type_detail_pool_( type_detail_pool )
        {}


        RILL_TV_OP_CONST( ctfe_engine, ast::term_expression, e, parent_env )
        {
            //
            llvm::Value* gen_val
                = ir_generator_->dispatch( e, parent_env );
            assert( gen_val != nullptr );

            llvm::Value* pure_val_ptr
                = reinterpret_cast<llvm::Value*>( reinterpret_cast<std::uintptr_t>( gen_val ) & ~1 );

            if ( ( reinterpret_cast<std::uintptr_t>( gen_val ) & 0x1 ) == 0 ) {
                // pure value
                return pure_val_ptr;

            } else {
                llvm::ConstantInt const* const type_id_value_ptr
                    = static_cast<llvm::ConstantInt const* const>( pure_val_ptr );

                type_id_t const type_id
                    = type_id_t( type_id_value_ptr->getZExtValue() );

                std::cout << "tpre!! " << type_id << std::endl;

                return type_detail_pool_->construct(
                    type_id,
                    nullptr //variable_env
                    );
            }

            return nullptr;
        }

    } // namespace compile_time
} // namespace rill
