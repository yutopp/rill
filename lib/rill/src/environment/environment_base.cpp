//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // debug

#include <rill/environment/environment.hpp>
#include <rill/environment/global_environment.hpp>
#include <rill/environment/root_environment.hpp>

#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    auto environment_unit::connect_from_ast( ast::const_ast_base_ptr const& ast )
        -> void
    {
        std::cout << "connect_from ast_id: " << ast->get_id()
                  << " -> env_id: " << get_id() << std::endl;

        b_.lock()->connect_from_ast( ast, shared_from_this() );
    }

    auto environment_unit::connect_to_ast( ast::statement_ptr const& ast )
        -> void
    {
        std::cout << "connect_to env_id: " << get_id()
                  << " -> ast_id: " << ast->get_id() << std::endl;

        b_.lock()->connect_to_ast( get_id(), ast );
    }

    auto environment_unit::get_related_ast()
        -> ast::statement_ptr
    {
        return b_.lock()->get_related_ast( get_id() );
    }

    auto environment_unit::get_related_ast() const
        -> ast::const_statement_ptr
    {
        return b_.lock()->get_related_ast( get_id() );
    }



    auto environment_base::lookup( ast::const_identifier_value_base_ptr const& identifier )
        -> env_base_pointer
    {
        // find symbol in self environment
        auto const s = find_on_env( identifier );

        // if found, return it.
        // otherwise
        //  if self is root(= has no more parent scope), return nullptr(FAILED).
        //  otherwise, try to find in parent scope.
        return s
            ? s
            : is_root()
                ? nullptr   // Not found...
                : get_parent_env()->lookup( identifier )
            ;
    }

    auto environment_base::lookup( ast::const_identifier_value_base_ptr const& identifier ) const
        -> const_env_base_pointer
    {
        auto const s = find_on_env( identifier );

        return s
            ? s
            : is_root()
                ? nullptr
                : get_parent_env()->lookup( identifier )
            ;
    }

    auto environment_base::find_on_env( ast::const_identifier_value_base_ptr const& identifier )
        -> env_base_pointer
    {
        auto const& name = identifier->get_inner_symbol()->to_native_string();

        // try to find in inner_envs_
        auto&& it = inner_envs_.find( name );
        if ( it != inner_envs_.end() ) {
            return cast_to_base( it->second );
        }

        // failed...
        return nullptr;
    }

    auto environment_base::find_on_env( ast::const_identifier_value_base_ptr const& identifier ) const
        -> const_env_base_pointer
    {
        auto const& name = identifier->get_inner_symbol()->to_native_string();

        auto&& it = inner_envs_.find( name );
        if ( it != inner_envs_.end() ) {
            return cast_to_base( it->second );
        }

        return nullptr;
    }

    auto environment_base::import_from(
        const_environment_base_ptr const& from
        )
        -> void
    {
        bool import_as_public = false;

        for( auto&& env_unit : from->inner_envs_ ) {
            auto const& name = std::get<0>( env_unit );
            auto const& target_env = std::get<1>( env_unit );

            std::cout << "import: " << name << std::endl;
            if ( target_env->is_private() ) {
                std::cout << "  - private" << std::endl;
                continue;
            }

            auto alias_env
                = b_.lock()->template allocate_env<alias_environment>(
                    shared_from_this()
                    );

            // copy!
            inner_envs_[name] = alias_env;
        }
    }


} // namespace rill
