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
        return nullptr;
    }


    auto single_identifier_environment_base::incomplete_construct(
        kind::variable_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& name
        )
        -> variable_symbol_environment_ptr
    {
        return nullptr;
    }


    auto single_identifier_environment_base::construct(
        kind::variable_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable...
        const_class_symbol_environment_ptr const& class_env,
        attribute::type_attributes const& type_attr
        )
        -> variable_symbol_environment_ptr
    {
        // FIXME:
        // create new environment
        auto const& v_env = allocate_env<variable_symbol_environment>( shared_from_this() );

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
