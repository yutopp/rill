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
        ast::identifier_value_base_ptr const& variable_name,
        ast::statement_ptr const& ast
        )
        -> variable_symbol_environment_ptr
    {
        auto const variable_env = incomplete_construct( kind::k_variable, variable_name );

        std::cout << "%% Marked(variable) " << variable_env->get_id() << std::endl;

        if ( ast != nullptr ) {
            std::cout << "%% Linked(variable) " << variable_env->get_id() << std::endl;
            variable_env->link_with_ast( ast );
        }

        //
        return variable_env;
    }


    auto environment_base::incomplete_construct(
        kind::variable_tag,
        ast::identifier_value_base_ptr const& variable_name
        )
        -> variable_symbol_environment_ptr
    {
        assert( variable_name != nullptr );

        auto const& symbol_name
            = variable_name->get_inner_symbol()->to_native_string();

        auto const& v_env
            = allocate_env_unless_exist<variable_symbol_environment>(
                symbol_name,
                symbol_name
                );

        assert( v_env->get_symbol_kind() == kind::type_value::e_variable );

        return v_env;
    }


    auto environment_base::construct(
        kind::variable_tag,
        ast::identifier_value_base_ptr const& variable_name,
        ast::statement_ptr const& ast,
        const_class_symbol_environment_ptr const& class_env,
        attribute::type_attributes const& type_attr
        )
        -> variable_symbol_environment_ptr
    {
        assert( variable_name != nullptr );

        //
        auto const v_env = mark_as( kind::k_variable, variable_name, ast );

        auto const& symbol_name
            = variable_name->get_inner_symbol()->to_native_string();

        // complete return type, name
        auto const& type_id = v_env->make_type_id( class_env, type_attr );
        v_env->complete( type_id, symbol_name );

        std::cout << "instanced!: " << symbol_name << std::endl;

        return v_env;
    }

} // namespace rill
