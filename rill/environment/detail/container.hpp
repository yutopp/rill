//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_CONTAINER
#define RILL_ENVIRONMENT_CONTAINER

#include <vector>
#include <memory>

#include "../../environment_fwd.hpp"


namespace rill
{
    // environment is managed by global id.
    // allocalor & table[env_id -> env_ptr]
    template<typename BaseEnvT>
    class environment_container
    {
    public:
        template<typename Env>
        struct result
        {
            environment_id_t id;
            std::shared_ptr<Env> pointer;
        };

        //
        template<typename Env, typename... T>
        auto allocate( T&&... ts )
            -> result<Env>
        {
            environment_id_t const next_id = nodes_.size();
            if ( next_id == environment_id_limit )
                assert( false );

            auto const p = std::make_shared<Env>( next_id, std::forward<T>( ts )... );
            result<Env> r = { next_id, p };

            nodes_.push_back( p );

            return r;
        }

        //
        auto at( environment_id_t const& id )
            -> std::weak_ptr<BaseEnvT>
        {
            assert( id >= 0 && id < nodes_.size() );
            return nodes_.at( id );
        }

        //
        auto at( environment_id_t const& id ) const
            -> std::weak_ptr<BaseEnvT const>
        {
            assert( id >= 0 && id < nodes_.size() );
            return nodes_.at( id );
        }

    private:
        std::vector<std::weak_ptr<BaseEnvT>> nodes_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_CONTAINER*/
