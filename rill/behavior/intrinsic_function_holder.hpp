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
#include <string>
#include <unordered_map>

#include <boost/optional.hpp>

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

        template<typename Action>
        auto append( std::string const& tag_name )
            -> intrinsic_function_action_id_t
        {
            auto const next_id = actions_.size();
            actions_.push_back( std::make_shared<Action>() );

            tag_map_[tag_name] = next_id;

            return next_id;
        }

        auto is_registered( std::string const& tag_name )
            -> boost::optional<intrinsic_function_action_id_t>
        {
            auto const it = tag_map_.find( tag_name );
            if ( it == tag_map_.cend() ) {
                return boost::none;
            }

            return it->second;
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
        std::unordered_map<std::string, intrinsic_function_action_id_t> tag_map_;

        intrinsic_function_action_base_ptr initialize_action_;
    };

} // namespace rill

#endif /*RILL_BEHAVIOR_INTRINSIC_FUNCTION_HOLDER_HPP*/
