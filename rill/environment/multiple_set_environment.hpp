//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_MULTIPLE_SET_ENVIRONMENT_HPP
#define RILL_ENVIRONMENT_MULTIPLE_SET_ENVIRONMENT_HPP

#include <memory>
#include <map>
#include <utility>

#include "environment_base.hpp"
#include "global_environment.hpp"


namespace rill
{
    //
    // This class denotes a set of environments.
    // This environment will not exist as "parent environment"(ignored by children)
    //
    class multiple_set_environment RILL_CXX11_FINAL
        : public environment_base
    {
    public:
        static kind::type_value const KindValue;

    public:
        multiple_set_environment( environment_parameter_t&& pp, native_string_type const& name )
            : environment_base( std::move( pp ) )
            , name_( name )
            , inner_env_symbol_kind_( kind::type_value::e_none )
        {}

    public:
        template<typename E, typename... Args>
        auto allocate_inner_env( Args&&... args )
            -> std::shared_ptr<E>
        {
            // NOTE: parant environment is not "this env" but "one rank top env"
            assert( has_parent() );

            // pass this environment's is and args...
            // inline environment will recieve id of this multiple_set_environment
            return b_.lock()->template allocate_env<E>(
                get_parent_env(),
                get_id(),
                std::forward<Args>( args )...
                );
        }

    public:
        auto get_name() const
            -> native_string_type const&
        {
            return name_;
        }

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_multi_set;
        }

        //
        auto get_representation_kind() const
            -> kind::type_value
        {
            return inner_env_symbol_kind_;
        }

        auto set_inner_env_symbol_kind( kind::type_value const& k )
            -> void
        {
            inner_env_symbol_kind_ = k;
        }

    public:
        template<typename To>
        auto get_unique_environment()
            -> std::shared_ptr<To>
        {
            auto const& normal_envs = get_normal_environments();
            if ( normal_envs.size() != 1 ) return nullptr;

            return cast_to<To>( normal_envs.at( 0 ) );
        }

        template<typename To>
        auto get_unique_environment() const
            -> std::shared_ptr<To const>
        {
            auto const& normal_envs = get_normal_environments();
            if ( normal_envs.size() != 1 ) return nullptr;

            return cast_to<To const>( normal_envs.at( 0 ) );
        }

    public:
        auto get_normal_environments() const
            -> std::vector<environment_base_ptr> const&
        {
            return normal_envs_;
        }

        auto get_template_environments() const
            -> std::vector<template_environment_ptr> const&
        {
            return template_envs_;
        }

        auto get_instanced_environments() const
            -> std::vector<environment_base_ptr> const&
        {
            return instanced_envs_;
        }

    public:
        auto add_to_normal_environments( environment_base_ptr const& env )
            -> void
        {
            return normal_envs_.push_back( env );
        }

        auto add_to_template_environments( template_environment_ptr const& env )
            -> void
        {
            return template_envs_.push_back( env );
        }

        auto add_to_instanced_environments(
            environment_base_ptr const& env,
            std::string const& cache_signature
            )
            -> void
        {
            instanced_cache_[cache_signature] = env;
            return instanced_envs_.push_back( env );
        }

        auto find_instanced_environments(
            std::string const& cache_signature
            ) const
            -> environment_base_ptr
        {
            auto it = instanced_cache_.find( cache_signature );
            if ( it == instanced_cache_.cend() ) {
                return nullptr;
            }

            return it->second;
        }

    private:
        native_string_type name_;

        kind::type_value inner_env_symbol_kind_;

        std::vector<environment_base_ptr> normal_envs_;
        std::vector<template_environment_ptr> template_envs_;

        std::vector<environment_base_ptr> instanced_envs_;
        std::unordered_map<std::string, environment_base_ptr> instanced_cache_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_MULTIPLE_SET_ENVIRONMENT_HPP*/
