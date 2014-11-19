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
#include "module_id.hpp"

#include "../type/type_registry.hpp"

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"


namespace rill
{
    class global_environment
        : public std::enable_shared_from_this<global_environment>
    {
    public:
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
        template<typename T, typename... Args>
        auto allocate_env( environment_unit_ptr const& base_env, Args&&... args )
        {
            return env_container_.template allocate<T>(
                shared_from_this(),
                base_env,
                base_env->get_owner_module_id(),
                base_env->is_private(),
                std::forward<Args>( args )...
                );
        }

        template<typename T, typename... Args>
        auto allocate_env_unless_exist(
            environment_base_ptr const& env,
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
        auto make_module( std::string const& name, boost::filesystem::path const& path )
            -> module_environment_ptr
        {
            if ( name_module_rel_.find( name ) != name_module_rel_.cend() ) {
                rill_dout << "module name: " << name << std::endl;
                assert( false && "[error] this module was already registered." );
            }

            //
            auto const module_id = module_counter_;
            ++module_counter_;

            auto mod
                = env_container_.template allocate<module_environment>(
                    shared_from_this(),
                    weak_environment_unit_ptr(),
                    module_id,
                    false,
                    path
                    );

            id_module_rel_[module_id] = mod;
            name_module_rel_[name] = mod;

            return mod;
        }

        auto find_module( std::string const& name ) const
            -> module_environment_ptr
        {
            if ( name_module_rel_.find( name ) == name_module_rel_.cend() ) {
                rill_dout << "module name: " << name << std::endl;
                assert( false && "[error] this module was NOT registered." );
            }

            return name_module_rel_.at( name );
        }


        ///
        /// Reference
        ///
        inline auto get_env_at( environment_id_t const& id )
            -> weak_environment_unit_ptr
        {
            return env_container_.at( id );
        }

        inline auto get_env_at( environment_id_t const& id ) const
            -> const_weak_environment_unit_ptr
        {
            return env_container_.at( id );
        }

        template<typename T>
        auto get_env_at_as_strong_ref( environment_id_t const& id )
            -> std::shared_ptr<T>
        {
            return cast_to<T>( get_env_at( id ).lock() );
        }

        auto get_env_at_as_strong_ref( environment_id_t const& id )
            -> std::shared_ptr<environment_base>
        {
            return cast_to_base( get_env_at( id ).lock() );
        }

        template<typename T>
        auto get_env_at_as_strong_ref( environment_id_t const& id ) const
            -> std::shared_ptr<T const>
        {
            return cast_to<T const>( get_env_at( id ).lock() );
        }

        auto get_env_at_as_strong_ref( environment_id_t const& id ) const
            -> std::shared_ptr<environment_base const>
        {
            return cast_to_base( get_env_at( id ).lock() );
        }

        ///
        /// AST rel
        ///
        inline auto connect_from_ast(
            ast::const_ast_base_ptr const& ast,
            environment_unit_ptr const& env
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


        ///
        /// Env rel
        ///
        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr )
            -> environment_base_ptr
        {
            auto const id
                = ast_to_env_id_map.get( ast_ptr );

            return id != environment_id_undefined
                ? get_env_at_as_strong_ref( id )
                : environment_base_ptr();

        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr ) const
            -> const_environment_base_ptr
        {
            auto const id
                = ast_to_env_id_map.get( ast_ptr );

            return id != environment_id_undefined
                ? get_env_at_as_strong_ref( id )
                : const_environment_base_ptr();
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
        auto get_related_type_id_from_ast_ptr( AstPtr const& ast_ptr ) const
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
        std::unordered_map<module_id_t, module_environment_ptr> id_module_rel_;
        std::unordered_map<std::string, module_environment_ptr> name_module_rel_;

        environment_registry<environment_unit> env_container_;
        ast_to_environment_id_mapper ast_to_env_id_map;
        environment_id_to_ast_mapper env_id_to_ast_map;

        type_registry types_container;
        ast_to_type_id_mapper ast_to_type_id_map;
    };

} // namespace rill
