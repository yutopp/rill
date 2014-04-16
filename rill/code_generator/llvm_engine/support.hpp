//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_LLVM_ENGINE_SUPPORT_HPP
#define RILL_CODE_GENERATOR_LLVM_ENGINE_SUPPORT_HPP

#include <memory>
#include <string>

#include <llvm/Target/TargetMachine.h>


namespace rill
{
    namespace code_generator
    {
        namespace llvm_engine
        {
            namespace detail
            {
                struct llvm_initializer;
                auto initialize_llvm() -> void;

            } // namespace detail

            auto make_default_target_machine( std::string& error_log )
                -> std::unique_ptr<llvm::TargetMachine>;

        } // namespace llvm_engine
    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_ENGINE_SUPPORT_HPP*/
