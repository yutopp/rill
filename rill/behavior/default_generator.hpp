//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_BEHAVIOR_DEFAULT_GENERATOR_HPP
#define RILL_BEHAVIOR_DEFAULT_GENERATOR_HPP

#include <memory>
#include <cassert>

#include "../environment/environment_fwd.hpp"
#include "../environment/class_symbol_environment.hpp"
#include "intrinsic_action_holder_fwd.hpp"


namespace rill
{
    namespace behavior
    {
        class default_system_info_setter_base
        {
        public:
            virtual ~default_system_info_setter_base() {}

        public:
            virtual auto get_align_and_size(
                class_builtin_kind const&
                ) const
                -> std::tuple<std::size_t, std::size_t> =0;
        };


        //
        template<typename Tag>
        class default_system_info_setter;


        //
        struct none_tag {};

        template<>
        class default_system_info_setter<none_tag>
            : public default_system_info_setter_base
        {
        public:
            virtual auto get_align_and_size(
                class_builtin_kind const& k
                ) const
                -> std::tuple<std::size_t, std::size_t> final override
            {
                return std::make_tuple( 0, 0 );
            }
        };


        //
        struct x86_64_tag {};

        template<>
        class default_system_info_setter<x86_64_tag>
            : public default_system_info_setter_base
        {
        public:
            virtual auto get_align_and_size(
                class_builtin_kind const& k
                ) const
                -> std::tuple<std::size_t, std::size_t> final override
            {
                switch( k ) {
                case class_builtin_kind::k_int8:
                    return std::make_tuple( 1, 1 ); // 1 Bytes alignment, 1 Bytes size

                case class_builtin_kind::k_int16:
                    return std::make_tuple( 2, 2 ); // 2 Bytes alignment, 2 Bytes size

                case class_builtin_kind::k_int32:
                    return std::make_tuple( 4, 4 ); // 4 Bytes alignment, 4 Bytes size

                case class_builtin_kind::k_float:
                    return std::make_tuple( 4, 4 ); // 4 Bytes alignment, 4 Bytes size

                case class_builtin_kind::k_bool:
                    return std::make_tuple( 1, 1 ); // 1 Bytes alignment, 1 Bytes size

                case class_builtin_kind::k_ptr:
                    return std::make_tuple( 8, 8 ); // 8 Bytes alignment, 8 Bytes size

                default:
                    assert( false );
                    return std::make_tuple( 0, 0 );
                }
            }
        };


        class total_system_info_setter
        {
        public:
            total_system_info_setter(
                std::shared_ptr<default_system_info_setter_base> const& host_setter,
                std::shared_ptr<default_system_info_setter_base> const& target_setter
                )
                : host_setter_( host_setter )
                , target_setter_( target_setter )
            {}

        public:
            // TODO: rename
            auto set(
                class_builtin_kind const& k,
                class_symbol_environment_ptr const& c_env
                ) const
                -> void
            {
                {
                    // host
                    auto const& t = host_setter_->get_align_and_size( k );
                    auto const& align = std::get<0>( t );
                    auto const& size = std::get<1>( t );

                    c_env->set_host_align( align );
                    c_env->set_host_size( size );
                }

                {
                    // target
                    auto const& t = target_setter_->get_align_and_size( k );
                    auto const& align = std::get<0>( t );
                    auto const& size = std::get<1>( t );

                    c_env->set_target_align( align );
                    c_env->set_target_size( size );
                }
            }

        private:
            std::shared_ptr<default_system_info_setter_base> host_setter_;
            std::shared_ptr<default_system_info_setter_base> target_setter_;
        };


        //
        void register_default_core(
            std::shared_ptr<intrinsic_action_holder> const&,
            std::shared_ptr<total_system_info_setter> const&
            );


        //
        class default_generator
        {
        public:
            void operator()(
                std::shared_ptr<intrinsic_action_holder> const& intrinsic_action_holder,
                std::shared_ptr<default_system_info_setter_base> const& host_setter
                    = std::make_shared<default_system_info_setter<none_tag>>(),
                std::shared_ptr<default_system_info_setter_base> const& target_setter
                    = std::make_shared<default_system_info_setter<none_tag>>()
                ) const
            {
                auto const sis
                    = std::make_shared<total_system_info_setter>(
                        host_setter,
                        target_setter
                        );

                register_default_core( intrinsic_action_holder, sis );
            }
        };
    } // namespace behavior
} // namespace rill

#endif /*RILL_BEHAVIOR_DEFAULT_GENERATOR_HPP*/
