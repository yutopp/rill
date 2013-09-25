#pragma once

#include <vector>

#include "embedded_function_holder_fwd.hpp"

#include "ast/value_fwd.hpp"
#include "interpreter/runtime.hpp"
#include "code_generator/llvm_ir_generator.hpp"


namespace rill
{
    class embedded_function_action_base
    {
    public:
        virtual ~embedded_function_action_base() {}

    public:
        virtual auto invoke( processing_context::debug_interpreter_tag, interpreter::context_ptr const& context ) const
            -> ast::intrinsic::value_base_ptr
        {
            return nullptr;
        }

        virtual auto invoke(
            processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
            std::shared_ptr<llvm::Module> const& module,
            std::shared_ptr<llvm::IRBuilder<>> const& builder,
            std::shared_ptr<code_generator::llvm_ir_generator::env_id_llvm_table> const& llvm_table,
            std::vector<environment_id_t> const& parameter_variable_decl_env_ids
            ) const
            -> llvm::Value*
        {
            return nullptr;
        }
    };


    class embedded_function_holder
    {
    public:
        template<typename Action>
        auto append()
            -> embedded_function_action_id_t
        {
            auto const next_id = actions_.size();
            actions_.push_back( std::make_shared<Action>() );

            return next_id;
        }

        auto at( embedded_function_action_id_t const& id )
            -> embedded_function_action_base_ptr
        {
            return actions_.at( id );
        }

        auto at( embedded_function_action_id_t const& id ) const
            -> const_embedded_function_action_base_ptr
        {
            return actions_.at( id );
        }

    private:
        std::vector<embedded_function_action_base_ptr> actions_;
    };
};
