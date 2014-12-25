//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_TYPE_TYPE_DETAIL_FACTORY_HPP
#define RILL_TYPE_TYPE_DETAIL_FACTORY_HPP

#include <memory>

#include "../environment/environment_base.hpp"
#include "../environment/global_environment.hpp"
#include "type_detail_pool_t.hpp"
#include "type_detail.hpp"


namespace rill
{
    class type_detail_factory
    {
    public:
        type_detail_factory(
            global_environment_ptr const& g_env,
            std::shared_ptr<type_detail_pool_t> const& pool,
            std::shared_ptr<raw_value_holder_pool_t> const& b_pool
            )
            : g_env_( g_env )
            , pool_( pool )
            , b_pool_( b_pool )
        {}

    public:
        template<typename... Args>
        auto change_attributes( type_detail_ptr const& ty_d, Args&&... attrs )
            -> type_detail_ptr
        {
            auto ty = g_env_->get_type_at( ty_d->type_id );
            attribute::detail::set( ty.attributes, attrs... );

            auto new_type_id = g_env_->make_type_id( ty.class_env_id, ty.attributes );
            return pool_->construct(
                new_type_id,
                ty_d->target_env,
                ty_d->nest,
                ty_d->template_args
                );
        }

        template<typename... Args>
        auto construct_type_detail( Args&&... args )
        {
            return pool_->construct( std::forward<Args>( args )... );
        }

        template<typename... Args>
        auto construct_raw_value_holder( Args&&... args )
        {
            return b_pool_->construct( std::forward<Args>( args )... );
        }

    private:
        global_environment_ptr g_env_;
        std::shared_ptr<type_detail_pool_t> pool_;
        std::shared_ptr<raw_value_holder_pool_t> b_pool_;
    };

} // namespace rill

#endif /*RILL_TYPE_TYPE_DETAIL_FACTORY_HPP*/
