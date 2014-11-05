//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <cassert>
#include <memory>
#include <unordered_map>
#include <bitset>
#include <vector>
#include <utility>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/iterator_range.hpp>

#include <boost/algorithm/string/join.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"
#include "environment_kind.hpp"
#include "environment_base.hpp"

#include "detail/mapper.hpp"

#include "root_environment.hpp"
#include "environment_registry.hpp"

#include "../type/type_registry.hpp"

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"


namespace rill
{

    template<typename BaseEnvT>
    struct environment_shared_resource
    {
        typedef environment_registry<BaseEnvT>      environment_registry_type;
        typedef ast_to_environment_id_mapper        ast_to_env_id_mapper_type;
        typedef environment_id_to_ast_mapper        env_id_to_ast_mapper_type;

        typedef type_registry                       type_registry_type;
        typedef ast_to_type_id_mapper               ast_to_type_id_mapper_type;

        environment_registry_type container;
        ast_to_env_id_mapper_type ast_to_env_id_map;
        env_id_to_ast_mapper_type env_id_to_ast_map;

        type_registry_type types_container;
        ast_to_type_id_mapper_type ast_to_type_id_map;
    };

    class global_environment;
    using global_environment_ptr = std::shared_ptr<global_environment>;
    using const_global_environment_ptr = std::shared_ptr<global_environment const>;

    using weak_global_environment_ptr = std::weak_ptr<global_environment>;

    //
    class global_environment
        : public std::enable_shared_from_this<global_environment>
    {
    public:
        typedef environment_base                        env_type;

        typedef std::shared_ptr<env_type>               env_base_pointer;
        typedef std::shared_ptr<env_type const>         const_env_base_pointer;
        typedef std::weak_ptr<env_type>                 weak_env_base_pointer;
        typedef std::weak_ptr<env_type const>           const_weak_env_base_pointer;

        typedef ast::native_string_t                    native_string_type;
        typedef environment_shared_resource<env_type>   shared_resource_type;

        using module_id_t = std::size_t;
        module_id_t module_counter_;
        std::unordered_map<module_id_t, env_base_pointer> id_module_rel_;
        std::unordered_map<std::string, env_base_pointer> name_module_rel_;

    public:
        global_environment()
            : module_counter_( 0 )
            , root_shared_resource_( std::make_shared<environment_shared_resource<env_type>>() )
        {
            std::cout << ">> global env constructed" << std::endl;
        }


        ~global_environment()
        {
            std::cout << "<< global env destructed"  << std::endl;
        }

    public:
        template<typename T, typename EnvPtr, typename... Args>
        auto allocate_env( EnvPtr const& base_env, Args&&... args )
            -> typename shared_resource_type::environment_registry_type::template result<T>::type
        {
#if 0
            bool const forward_referenceable
                = do_mark_child_env_as_forward_referenceable_;
            std::size_t const decl_order
                = forward_referenceable ? 0 : (*next_child_env_order_)++;
            std::shared_ptr<std::size_t> const& next_child_env_order
                = do_mark_child_env_as_forward_referenceable_ ? nullptr : next_child_env_order_;
#else
            bool const forward_referenceable = true;
            std::size_t const decl_order = 0;
            std::shared_ptr<std::size_t> const& next_child_env_order = nullptr;
#endif
            assert( root_shared_resource_ != nullptr );

            return root_shared_resource_->container.template allocate<T>(
                shared_from_this(),
                base_env,
                forward_referenceable,
                decl_order,
                false/*do_mark_child_env_as_forward_referenceable_*/,
                next_child_env_order,
                std::forward<Args>( args )...
                );
        }

        auto make_module( std::string const& name = "" )
            -> env_base_pointer
        {
            auto const decl_module_id = 0;
            auto const owner_module_id = 0;

            auto const module_id = module_counter_;
            ++module_counter_;

            auto mod
                = root_shared_resource_->container.template allocate_root<root_environment>( shared_from_this() );

            id_module_rel_[module_id] = mod;
            name_module_rel_[name] = mod;

            return mod;
        }

        auto find_module( std::string const& name ) const
            -> env_base_pointer
        {
            return name_module_rel_.at( name );
        }




        auto get_env_at( environment_id_t const& id )
            -> weak_env_base_pointer
        {
            return root_shared_resource_->container.at( id );
        }

        auto get_env_at( environment_id_t const& id ) const
            -> const_weak_env_base_pointer
        {
            return root_shared_resource_->container.at( id );
        }


        inline auto get_env_at_as_strong_ref( environment_id_t const& id )
            -> env_base_pointer
        {
            return get_env_at( id ).lock();
        }
        inline auto get_env_at_as_strong_ref( environment_id_t const& id ) const
            -> const_env_base_pointer
        {
            return get_env_at( id ).lock();
        }

        template<typename T>
        auto get_env_at_as_strong_ref( environment_id_t const& id )
            -> std::shared_ptr<T>
        {
            return cast_to<T>( get_env_at_as_strong_ref( id ) );
        }
        template<typename T>
        auto get_env_at_as_strong_ref( environment_id_t const& id ) const
            -> std::shared_ptr<T const>
        {
            return cast_to<T const>( get_env_at_as_strong_ref( id ) );
        }


        auto get_env_strong_at( environment_id_t const& id )
            -> env_base_pointer
        {
            return get_env_at( id ).lock();
        }

        auto get_env_strong_at( environment_id_t const& id ) const
            -> const_env_base_pointer
        {
            return get_env_at( id ).lock();
        }



        ///
        ///
        ///
        virtual auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream&
        { return os; }




        //
        //
        //
        auto make_type_id(
            attribute::type_attributes const& type_attr = attribute::make_empty_type_attributes()
            ) const
            -> shared_resource_type::type_registry_type::type_id_type
        {
            return make_type_id( nullptr, type_attr );
        }

        auto make_type_id(
            const_class_symbol_environment_ptr const& e,
            attribute::type_attributes const& type_attr = attribute::make_default_type_attributes()
            ) const
            -> shared_resource_type::type_registry_type::type_id_type
        {
            // TODO: DUPLICATE CHECK!!!
            return root_shared_resource_->types_container.add( e, type_attr );
        }

        auto make_type_id(
            environment_id_t const& id,
            attribute::type_attributes const& type_attr = attribute::make_default_type_attributes()
            ) const
            -> shared_resource_type::type_registry_type::type_id_type
        {
            // TODO: DUPLICATE CHECK!!!
            return root_shared_resource_->types_container.add( id, type_attr );
        }

        auto get_type_at(
            shared_resource_type::type_registry_type::type_id_type const& type_id
            ) const
            -> shared_resource_type::type_registry_type::type_type const&
        {
            return root_shared_resource_->types_container.at( type_id );
        }


        template<typename AstPtr>
        auto bind_type_id_with_ast( AstPtr const& ast_ptr, type_id_t const& tid )
            -> void
        {
            root_shared_resource_->ast_to_type_id_map.add( ast_ptr, tid );
        }



        template<typename AstPtr>
        auto get_related_type_id_by_ast_ptr( AstPtr const& ast_ptr ) const
            -> type_id_t
        {
            return root_shared_resource_->ast_to_type_id_map.get( ast_ptr );
        }

        //
        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr )
            -> env_base_pointer
        {
            auto const id
                = root_shared_resource_->ast_to_env_id_map.get( ast_ptr );

            return ( id != environment_id_undefined )
                ? get_env_at( id ).lock()
                : env_base_pointer();
        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr ) const
            -> const_env_base_pointer
        {
            auto const id
                = root_shared_resource_->ast_to_env_id_map.get( ast_ptr );

            return ( id != environment_id_undefined )
                ? get_env_at( id ).lock()
                : env_base_pointer();
        }

    public:
        template<typename T, typename EnvPtr, typename... Args>
        auto allocate_env_unless_exist(
            EnvPtr const& env,
            native_string_type const& name,
            Args&&... args
            )
            -> std::shared_ptr<T>
        {
            if ( !env->is_exist( name ) ) {
                // make new incomplete env
                auto const& w_env = allocate_env<T>( env, std::forward<Args>( args )... );
                assert( w_env != nullptr );

                env->inner_envs_[name] = w_env;
                return std::dynamic_pointer_cast<T>( w_env );

            } else {
                auto const& w_env = env->inner_envs_[name];
                assert( w_env != nullptr );

                return std::dynamic_pointer_cast<T>( w_env );
            }
        }


    public:
        std::shared_ptr<shared_resource_type> root_shared_resource_;
    };

} // namespace rill
