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
        template<
            typename Node,
            typename ActionHolderPtr,
            typename EnvPtr = environment_base_ptr
            >
        auto generate_llvm_ir(
            global_environment_ptr const& g_env,
            Node const& node,
            ActionHolderPtr const& action_holder,
            EnvPtr const& env = nullptr
            )
        {
            auto const& context = std::make_shared<llvm_ir_generator_context>();

            llvm_ir_generator ir_generator( g_env, action_holder, context );
            ir_generator.dispatch( node, env );

            ir_generator.debug();

            return context;
        }

    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_HPP*/
