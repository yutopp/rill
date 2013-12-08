//
// Copyright yutopp 2013 - .
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
    // ====
    // Variable environment construction
    // ====

    auto environment_base::mark_as(
        kind::variable_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& variable_name,
        ast::statement_ptr const& ast
        )
        -> decltype( static_cast<environment_base *>( nullptr )->incomplete_construct( kind::k_variable, variable_name ) )
    {
        auto const& variable_env = incomplete_construct( kind::k_variable, variable_name );

        std::cout << "%% Marked(class) " << variable_env->get_id() << std::endl;

        if ( ast != nullptr ) {
            variable_env->link_with_ast( ast );
        }

        //
        return variable_env;
    }


    auto single_identifier_environment_base::incomplete_construct(
        kind::variable_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& variable_name
        )
        -> variable_symbol_environment_ptr
    {
        auto const& v_env = [&]() -> variable_symbol_environment_ptr {
            if ( variable_name != nullptr ) {
                auto const& symbol_name = variable_name->get_inner_symbol()->to_native_string();

                if ( !is_instanced( symbol_name ) ) {
                    // make new incomplete env
                    auto const& i_env = allocate_env<variable_symbol_environment>( shared_from_this() );
                    instanced_env_[symbol_name] = i_env;
                }

                auto const& env = instanced_env_.at( symbol_name );
                assert( env != nullptr );
                assert( env->get_symbol_kind() == kind::type_value::e_variable );

                return std::static_pointer_cast<variable_symbol_environment>( env );

            } else {
                auto const& i_env = allocate_env<variable_symbol_environment>( shared_from_this() );
                native_string_type const& symbol_name
                    = variable_name
                    ? variable_name->get_inner_symbol()->to_native_string()
                    : "__unnamed" + std::to_string( i_env->get_id() )
                    ;

                if ( is_instanced( symbol_name ) ) {
                    // Meybe ICE...
                    assert( false );
                }

                instanced_env_[symbol_name] = i_env;
                return i_env;
            }
        }();

        return v_env;
    }


    auto single_identifier_environment_base::construct(
        kind::variable_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable...
        ast::statement_ptr const& ast,
        const_class_symbol_environment_ptr const& class_env,
        attribute::type_attributes const& type_attr
        )
        -> variable_symbol_environment_ptr
    {
        // FIXME:
        // create new environment
        auto const& v_env = mark_as( kind::k_variable, variable_name, ast );

        native_string_type const& symbol_name
            = variable_name
            ? variable_name->get_inner_symbol()->to_native_string()
            : "__unnamed" + std::to_string( v_env->get_id() )
            ;

        // complete return type, name
        auto const& type_id = v_env->make_type_id( class_env, type_attr );
        v_env->complete( type_id, symbol_name );

        std::cout << "instanced!: " << symbol_name << std::endl;

        instanced_env_[symbol_name] = v_env;
        return v_env;
    }

} // namespace rill
