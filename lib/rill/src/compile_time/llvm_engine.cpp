//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine.hpp>
#include <rill/compile_time/engine_value_holder.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace compile_time
    {
        llvm_engine::llvm_engine(
            std::shared_ptr<code_generator::llvm_ir_generator> const& generator,
            std::shared_ptr<llvm::ExecutionEngine> const& engine
            )
            : generator_( generator )
            , engine_( engine )
            , value_holder_( std::make_shared<engine_value_holder>() )
        {}


        RILL_TV_OP_CONST( llvm_engine, ast::term_expression, e, parent_env )
        {
            //
            llvm::Value* val = generator_->dispatch( e, parent_env );
            return nullptr;
        }

    } // namespace compile_time
} // namespace rill
