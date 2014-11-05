//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_REGISTRY_HPP
#define RILL_ENVIRONMENT_REGISTRY_HPP

#include <vector>
#include <memory>

#include "environment_fwd.hpp"


namespace rill
{
    // environment is managed by global id.
    // allocalor & table[env_id -> env_ptr]
    template<typename BaseEnvT>
    class environment_registry
    {
    public:
        template<typename Env>
        struct result
        {
            typedef std::shared_ptr<Env> type;
        };

        //
        template<typename TargetEnv, typename... Args>
        auto allocate_root(
            Args&&... args
            )
            -> typename result<TargetEnv>::type
        {
            environment_id_t const next_id = environment_id_t( nodes_.size() );
            if ( next_id == environment_id_limit )
                assert( false );

            auto const p = std::make_shared<TargetEnv>(
                std::forward<Args>( args )...
                );
            nodes_.push_back( p );

            return p;
        }

        //
        template<typename TargetEnv, typename... Args>
        auto allocate(
            weak_global_environment_ptr const& global_env,
            weak_environment_base_ptr const& parent,
            bool const forward_referenceable,
            std::size_t const& decl_order,
            bool const do_mark_child_env_as_forward_referenceable,
            std::shared_ptr<std::size_t> const& next_child_env_order,
            Args&&... args
            )
            -> typename result<TargetEnv>::type
        {
            environment_id_t const next_id = environment_id_t( nodes_.size() );
            if ( next_id == environment_id_limit )
                assert( false );

            environment_parameter_t params = {
                global_env,
                next_id,
                parent,
                forward_referenceable,
                decl_order,
                do_mark_child_env_as_forward_referenceable,
                next_child_env_order
            };

            auto const p = std::make_shared<TargetEnv>(
                std::move( params ),
                std::forward<Args>( args )...
                );
            nodes_.push_back( p );

            return p;
        }

        //
        auto at( environment_id_t const& id )
            -> std::weak_ptr<BaseEnvT>
        {
            assert( id < nodes_.size() );
            return nodes_.at( id );
        }

        //
        auto at( environment_id_t const& id ) const
            -> std::weak_ptr<BaseEnvT const>
        {
            assert( id >= environment_id_t( 0 ) && id < environment_id_limit );
            return nodes_.at( id );
        }

    private:
        std::vector<std::weak_ptr<BaseEnvT>> nodes_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_REGISTRY_HPP*/
