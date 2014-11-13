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

#include <rill/utility/tie.hpp>


namespace rill
{
    // ====
    // template environment construction
    // ====

    auto environment_base::mark_as(
        kind::template_tag,
        ast::identifier_value_base_ptr const& name_identifier,
        ast::statement_ptr const& ast
        )
        -> std::pair<multiple_set_environment_ptr, template_environment_ptr>
    {
        // construct incomplete environment( template_set & template )
        RILL_PP_TIE(
            multi_set_env, template_env,
            incomplete_construct( kind::template_tag(), name_identifier )
            );

        rill_dregion {
            std::cout << "Template Marked, %&%& " << multi_set_env->get_id()
                      << " : " << template_env->get_id() << std::endl;
        }

        //
        template_env->link_with_ast( ast );

        return std::make_pair(
            std::move( multi_set_env ),
            std::move( template_env )
            );
    }




    //
    //
    //
    auto environment_base::incomplete_construct(
        kind::template_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> std::pair<multiple_set_environment_ptr, template_environment_ptr>
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // wrapper environment
        auto const& set_environment
            = b_.lock()->allocate_env_unless_exist<multiple_set_environment>(
                std::static_pointer_cast<environment_base>( shared_from_this() ),
                symbol_name,
                symbol_name
                );

        // allocate incomplete funciton environment
        auto const& incomplete_template_env
            = set_environment->allocate_inner_env<template_environment>();

        return std::make_pair(
            std::move( set_environment ),
            std::move( incomplete_template_env)
            );
    }

#if 0

    //
    //
    //
    auto environment_base::construct(
            kind::template_tag,
            ast::identifier_value_base_ptr const& function_name,
            ast::statement_ptr const& ast,
            function_env_generator_scope_type const& parameter_decl_initializer,
            class_symbol_environment_ptr const& return_class_env,
            attribute::type_attributes const& return_type_attr
            ) -> function_symbol_environment_ptr
    {
        auto const& p_i_pair = mark_as( kind::k_function, function_name, ast );
        auto const& parameter_env = p_i_pair.first;
        auto const& incomplete_function_env = p_i_pair.second;

        auto const& symbol_name = function_name->get_inner_symbol()->to_native_string();

        // complete parameter decl
        auto const& parameter_completed_function_env_pointer = parameter_decl_initializer( incomplete_function_env );

        // complete return type, name
        auto const& return_type_id = b_.lock()->make_type_id( return_class_env, return_type_attr );
        parameter_completed_function_env_pointer->complete( return_type_id, symbol_name );

        //
        parameter_env->add_overload( parameter_completed_function_env_pointer );

        return parameter_completed_function_env_pointer;
    }
#endif

} // namespace rill
