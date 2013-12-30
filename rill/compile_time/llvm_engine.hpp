//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_LLVM_ENGINE_HPP
#define RILL_COMPILE_TIME_LLVM_ENGINE_HPP

#include <memory>

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include "../ast/detail/tree_visitor_base.hpp"

#include "../behavior/intrinsic_function_holder_fwd.hpp"
#include "../code_generator/llvm_ir_generator.hpp"

#include "engine_value_holder_fwd.hpp"


namespace rill
{
    namespace compile_time
    {
        // this class holds llvm_ir_generator and llvm_ir_execute_engine
        class llvm_engine RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<llvm_engine, std::shared_ptr<void>>
        {
        public:
            llvm_engine(
                std::shared_ptr<code_generator::llvm_ir_generator> const& generator,
                std::shared_ptr<llvm::ExecutionEngine> const& engine
                );

        public:
            RILL_TV_OP_DECL_CONST( ast::term_expression )

            RILL_TV_OP_FAIL

        private:
            std::shared_ptr<code_generator::llvm_ir_generator> generator_;
            std::shared_ptr<llvm::ExecutionEngine> engine_;
            std::shared_ptr<engine_value_holder> value_holder_;
        };



        //
        template<typename EnvironmentPtr, typename ActionHolderPtr, typename Node>
        auto make_llvm_engine(
            EnvironmentPtr const& env,
            ActionHolderPtr const& action_holder,
            std::shared_ptr<Node> const& node
            )
            -> std::shared_ptr<llvm_engine const>
        {
            // first, create llvm_ir_generator
            auto const& context
                = std::make_shared<code_generator::llvm_ir_generator_context>();

            action_holder->invoke_initialize_action(
                processing_context::k_llvm_ir_generator,
                context,
                env
                );

            auto const& generator
                = std::make_shared<code_generator::llvm_ir_generator>(
                    env,
                    action_holder,
                    context
                    )
                ;

            // second, create llvm JIT Engine
            // the ownership of context.llvm_module is moved to engine.
            // FIXME: the destructor of llvm::ExecutionEngine will execute "delete ptr to module", so this "&context->llvm_module" can be dangerous. MUST BE CHANGE.(cause Undefined behaviour)
            auto const engine
                = std::shared_ptr<llvm::ExecutionEngine>(
                    llvm::EngineBuilder( &context->llvm_module ).create() \
                    );

            return std::make_shared<llvm_engine const>( generator, engine );
        }



    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_HPP*/
