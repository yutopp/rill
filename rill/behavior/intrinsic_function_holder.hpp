//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_HPP
#define RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_HPP

#include <vector>

#include "intrinsic_function_holder_fwd.hpp"

#include "../environment/environment_fwd.hpp"

#include "../ast/value_fwd.hpp"
#include "../code_generator/llvm_ir_generator.hpp"


namespace rill
{
    class intrinsic_function_action_base
    {
    public:
        virtual ~intrinsic_function_action_base() {}

    public:
/*
        virtual auto invoke(
            processing_context::debug_interpreter_tag,
            interpreter::context_ptr const& context
            ) const
            -> ast::intrinsic::value_base_ptr
        {
            return nullptr;
        }
*/
        virtual auto invoke(
            processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
            code_generator::llvm_ir_generator_context_ptr const& context,
            const_environment_base_ptr const& env,
            environment_id_list_t const& argument_var_env_ids = environment_id_list_t()
            ) const
            -> llvm::Value*
        {
            return nullptr;
        }
    };


    class intrinsic_function_action_holder
    {
    public:
        template<typename Action>
        auto append()
            -> intrinsic_function_action_id_t
        {
            auto const next_id = actions_.size();
            actions_.push_back( std::make_shared<Action>() );

            return next_id;
        }

        auto at( intrinsic_function_action_id_t const& id )
            -> intrinsic_function_action_base_ptr
        {
            return actions_.at( id );
        }

        auto at( intrinsic_function_action_id_t const& id ) const
            -> const_intrinsic_function_action_base_ptr
        {
            return actions_.at( id );
        }

        template<typename ActionPtr>
        void set_initialize_action( ActionPtr const& action )
        {
            assert( ( initialize_action_ == nullptr ) && "double assign" );
            initialize_action_ = action;
        }
        

        // TODO: invoke

        template<typename... Args>
        void invoke_initialize_action( Args&&... args )
        {
            if ( initialize_action_ ) {
                initialize_action_->invoke( std::forward<Args>( args )... );
            }

            // assert( false );
        }
    private:
        std::vector<intrinsic_function_action_base_ptr> actions_;
        intrinsic_function_action_base_ptr initialize_action_;
    };

} // namespace rill

#endif /*RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_HPP*/
