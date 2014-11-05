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
    using module_id_t = std::size_t;

    class global_environment
        : public std::enable_shared_from_this<global_environment>
    {
    public:
        using base_env_type = environment_base;
        using base_env_pointer = std::shared_ptr<base_env_type>;
        using const_base_env_pointer = std::shared_ptr<const base_env_type>;
        using weak_base_env_pointer = std::weak_ptr<base_env_type>;
        using const_weak_base_env_pointer = std::weak_ptr<const base_env_type>;

        using native_string_type = ast::native_string_t;

    public:
        global_environment()
            : module_counter_( 0 )
        {}

        global_environment( global_environment const& ) =delete;

    public:
        ///
        /// Allocation
        ///
        template<typename T, typename EnvPtr, typename... Args>
        auto allocate_env( EnvPtr const& base_env, Args&&... args )
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

            return container.template allocate<T>(
                shared_from_this(),
                base_env,
                forward_referenceable,
                decl_order,
                false/*do_mark_child_env_as_forward_referenceable_*/,
                next_child_env_order,
                std::forward<Args>( args )...
                );
        }

        template<typename T, typename EnvPtr, typename... Args>
        auto allocate_env_unless_exist(
            EnvPtr const& env,
            native_string_type const& name,
            Args&&... args
            )
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


        ///
        /// Module
        ///
        auto make_module( std::string const& name = "" )
            -> base_env_pointer
        {
            auto const decl_module_id = 0;
            auto const owner_module_id = 0;

            auto const module_id = module_counter_;
            ++module_counter_;

            auto mod
                = container.template allocate_root<root_environment>( shared_from_this() );

            id_module_rel_[module_id] = mod;
            name_module_rel_[name] = mod;

            return mod;
        }

        auto find_module( std::string const& name ) const
            -> base_env_pointer
        {
            return name_module_rel_.at( name );
        }


        ///
        /// Reference
        ///
        inline auto get_env_at( environment_id_t const& id )
            -> weak_base_env_pointer
        {
            return container.at( id );
        }

        inline auto get_env_at( environment_id_t const& id ) const
            -> const_weak_base_env_pointer
        {
            return container.at( id );
        }

        inline auto get_env_at_as_strong_ref( environment_id_t const& id )
            -> base_env_pointer
        {
            return get_env_at( id ).lock();
        }
        inline auto get_env_at_as_strong_ref( environment_id_t const& id ) const
            -> const_base_env_pointer
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


        ///
        /// AST rel
        ///
        inline auto connect_from_ast(
            ast::const_ast_base_ptr const& ast,
            base_env_pointer const& env
            )
            -> void
        {
            ast_to_env_id_map.add( ast, env );
        }

        inline auto connect_to_ast(
            environment_id_t const& id,
            ast::statement_ptr const& ast
            )
            -> void
        {
            env_id_to_ast_map.add( id, ast );
        }

        inline auto get_related_ast( environment_id_t const& id )
            -> ast::statement_ptr
        {
            // registered by connect_to_ast
            return env_id_to_ast_map.get( id );
        }

        inline auto get_related_ast( environment_id_t const& id ) const
            -> ast::const_statement_ptr
        {
            // registered by connect_to_ast
            return env_id_to_ast_map.get( id );
        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr )
            -> base_env_pointer
        {
            auto const id
                = ast_to_env_id_map.get( ast_ptr );

            return ( id != environment_id_undefined )
                ? get_env_at( id ).lock()
                : base_env_pointer();
        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr ) const
            -> const_base_env_pointer
        {
            auto const id
                = ast_to_env_id_map.get( ast_ptr );

            return ( id != environment_id_undefined )
                ? get_env_at( id ).lock()
                : base_env_pointer();
        }


        //
        // Types
        //
        auto make_type_id(
            attribute::type_attributes const& type_attr
                = attribute::make_empty_type_attributes()
            )
            -> type_id_t
        {
            return make_type_id( nullptr, type_attr );
        }

        auto make_type_id(
            const_class_symbol_environment_ptr const& e,
            attribute::type_attributes const& type_attr
                = attribute::make_default_type_attributes()
            )
            -> type_id_t
        {
            // TODO: DUPLICATE CHECK!!!
            return types_container.add( e, type_attr );
        }

        auto make_type_id(
            environment_id_t const& id,
            attribute::type_attributes const& type_attr
                = attribute::make_default_type_attributes()
            )
            -> type_id_t
        {
            // TODO: DUPLICATE CHECK!!!
            return types_container.add( id, type_attr );
        }

        auto get_type_at(
            type_id_t const& type_id
            ) const
            -> decltype(auto) // maybe, const ref
        {
            return types_container.at( type_id );
        }

        template<typename AstPtr>
        auto bind_type_id_with_ast( AstPtr const& ast_ptr, type_id_t const& tid )
            -> void
        {
            ast_to_type_id_map.add( ast_ptr, tid );
        }

        template<typename AstPtr>
        auto get_related_type_id_by_ast_ptr( AstPtr const& ast_ptr ) const
            -> type_id_t
        {
            return ast_to_type_id_map.get( ast_ptr );
        }


        ///
        /// debug
        ///
        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream&;

    private:
        module_id_t module_counter_;
        std::unordered_map<module_id_t, base_env_pointer> id_module_rel_;
        std::unordered_map<std::string, base_env_pointer> name_module_rel_;

        environment_registry<base_env_type> container;
        ast_to_environment_id_mapper ast_to_env_id_map;
        environment_id_to_ast_mapper env_id_to_ast_map;

        type_registry types_container;
        ast_to_type_id_mapper ast_to_type_id_map;
    };

} // namespace rill
