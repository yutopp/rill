//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_INTRINSIC_ACTION_BASE_HPP
#define RILL_BEHAVIOR_INTRINSIC_ACTION_BASE_HPP

#include <vector>

#include "intrinsic_action_holder_fwd.hpp"

#include "../environment/environment_fwd.hpp"
#include "../code_generator/llvm_ir_generator.hpp"


namespace rill
{
    namespace processing_context
    {
        class semantics_typing_tag {};
        auto const k_semantics_typing = semantics_typing_tag();

        class llvm_ir_generator_typing_tag {};
        auto const k_llvm_ir_generator_typing = llvm_ir_generator_typing_tag();

        class llvm_ir_generator_tag {};
        auto const k_llvm_ir_generator = llvm_ir_generator_tag();

    } // namespace processing_context


    class intrinsic_action_base
    {
    public:
        virtual ~intrinsic_action_base() {}

    public:
        virtual auto invoke(
            rill::processing_context::semantics_typing_tag,
            class_symbol_environment_ptr const& c_env
            ) const
            -> void
        {}

        virtual auto invoke(
            rill::processing_context::llvm_ir_generator_typing_tag,
            code_generator::llvm_ir_generator_context_ptr const& context,
            const_class_symbol_environment_ptr const& c_env
            ) const
            -> void
        {}

        virtual auto invoke(
            processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
            code_generator::llvm_ir_generator_context_ptr const& context,
            const_environment_base_ptr const& f_env,
            std::vector<llvm::Value*> const& argument_vars = std::vector<llvm::Value*>{}
            ) const
            -> llvm::Value*
        {
            return nullptr;
        }
    };

} // namespace rill

#endif /*RILL_BEHAVIOR_INTRINSIC_ACTION_BASE_HPP*/
