//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_HPP
#define RILL_CODE_GENERATOR_HPP

#include <memory>

#include "llvm_ir_generator.hpp"
#include "binary_generator_from_llvm_ir.hpp"


namespace rill
{
    namespace code_generator
    {
        template<typename EnvironmentPtr, typename ActionHolderPtr, typename Node>
        auto generate_llvm_ir(
            EnvironmentPtr const& env,
            ActionHolderPtr const& action_holder,
            std::shared_ptr<Node> const& node
            )
            -> void
        {
            auto const& context = std::make_shared<llvm_ir_generator_context>();

            // call intrinsic action initializer
            action_holder->invoke_initialize_action(
                processing_context::k_llvm_ir_generator,
                context,
                env
                );

            //
            llvm_ir_generator const ir_generator( env, action_holder, context );
            ir_generator.dispatch( node, env );

            //
            ir_generator.debug();

            // FIXME
            auto const binary_gen = binary_generator_from_llvm_ir( context );
            binary_gen.test();
        }

 
        template<typename EnvironmentPtr, typename ActionHolderPtr, typename Node>
        auto run_as_ctfe(
            EnvironmentPtr const& env,
            ActionHolderPtr const& action_holder,
            std::shared_ptr<Node> const& node
            )
            -> void
        {
            auto const& context = std::make_shared<llvm_ir_generator_context>();

            // call intrinsic action initializer
            action_holder->invoke_initialize_action(
                processing_context::k_llvm_ir_generator,
                context,
                env
                );

            //
            llvm_ir_generator const ir_generator( env, action_holder, context );
            ir_generator.dispatch( node, env );

            //
            ir_generator.debug();

            // FIXME
            auto const binary_gen = binary_generator_from_llvm_ir( context );
            binary_gen.test();
        }


//TheExecutionEngine = EngineBuilder(TheModule).create();



    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_HPP*/
