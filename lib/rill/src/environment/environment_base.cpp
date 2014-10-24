//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // debug

#include <rill/environment/environment.hpp>

#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
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

    auto environment_base::lookup_buildin_class( std::string const& name )
        -> class_symbol_environment_ptr
    {
        auto const& generic_type_class_set_env
            = lookup_on_root( ast::make_identifier( name ) );
        assert( generic_type_class_set_env != nullptr );  // literal type must exist

        auto const& type_class_set_env
            = cast_to<multiple_set_environment>( generic_type_class_set_env );

        assert( type_class_set_env != nullptr );

        auto const& ne
            = type_class_set_env->get_normal_environments();
        assert( ne.size() == 1 );

        return cast_to<class_symbol_environment>( ne.at( 0 ) );
    }

    auto environment_base::find_on_env( ast::const_identifier_value_base_ptr const& identifier )
        -> env_base_pointer
    {
        auto const& name = identifier->get_inner_symbol()->to_native_string();

        // try to find in inner_envs_
        auto&& it = inner_envs_.find( name );
        if ( it != inner_envs_.end() ) {
            return it->second;
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
            return it->second;
        }

        return nullptr;
    }

} // namespace rill
