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
    auto function_symbol_environment::parameter_variable_construct(
        ast::intrinsic::single_identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable
        const_class_symbol_environment_ptr const& class_env,
        attribute::type_attributes const& type_attr
        )
        -> variable_symbol_environment_ptr
    {
        // declare parameter variable
        auto const& var_env = construct( kind::k_variable, variable_name, nullptr, class_env, type_attr );
        parameter_decl_ids_.push_back( var_env->get_id() );
        parameter_type_ids_.push_back( var_env->get_type_id() );    // memo parameter variable types

        return var_env;
    }

} // namespace rill
