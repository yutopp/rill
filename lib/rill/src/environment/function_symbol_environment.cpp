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
    kind::type_value const function_symbol_environment::KindValue
        = kind::type_value::e_function;


    auto function_symbol_environment::parameter_variable_construct(
        ast::identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable
        const_class_symbol_environment_ptr const& class_env,
        attribute::type_attributes const& type_attr
        )
        -> variable_symbol_environment_ptr
    {
        std::cout << ">>> parameter_variable_construct! " << std::endl
                  << ">>> name : " << class_env->get_mangled_name() << std::endl;
        // declare parameter variable
        auto const& var_env = construct( kind::k_variable, variable_name, nullptr, class_env, type_attr );
        parameter_decl_ids_.push_back( var_env->get_id() );
        parameter_type_ids_.push_back( var_env->get_type_id() );    // memo parameter variable types

        return var_env;
    }


    auto function_symbol_environment::parameter_variable_construct(
        ast::identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable
        environment_id_t const& type_env_id,
        attribute::type_attributes const& type_attr
        )
        -> variable_symbol_environment_ptr
    {
        // TODO: add env kind check

        auto const& c_env
            = cast_to<class_symbol_environment const>(
                get_env_at_as_strong_ref( type_env_id )
                );
        assert( c_env != nullptr );

        // delegate
        return parameter_variable_construct(
            variable_name,
            c_env,
            type_attr
            );
    }

    auto function_symbol_environment::parameter_variable_construct(
        ast::identifier_value_base_ptr const& name,
        type_id_t const& type_id
        )
        -> variable_symbol_environment_ptr
    {
        auto const& t = get_type_at( type_id );
        return parameter_variable_construct( name, t.class_env_id, t.attributes );
    }

} // namespace rill
