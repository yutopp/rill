//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment/environment.hpp>

#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    kind::type_value const multiple_set_environment::KindValue
        = kind::type_value::e_multi_set;


    //
    auto multiple_set_environment::incomplete_construct(
        kind::function_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> function_symbol_environment_ptr
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // allocate incomplete funciton environment
        auto incomplete_function_env
            = allocate_inner_env<function_symbol_environment>(
                symbol_name
                );

        return incomplete_function_env;
    }

    auto multiple_set_environment::incomplete_construct(
        kind::class_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> class_symbol_environment_ptr
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // allocate incomplete funciton environment
        auto incomplete_class_env
            = allocate_inner_env<class_symbol_environment>(
                symbol_name
                );

        return incomplete_class_env;
    }

    auto multiple_set_environment::incomplete_construct(
        kind::template_tag
        )
        -> template_environment_ptr
    {
        // allocate incomplete funciton environment
        auto const& incomplete_template_env
            = allocate_inner_env<template_environment>();

        return incomplete_template_env;
    }

    auto multiple_set_environment::propagate_outer_referenced_ast(
        outer_referenced_ast_ptr_type const& node
        )
        -> void
    {
        rill_dout << "multi !!!!! " << std::endl;
        environment_base::propagate_outer_referenced_ast( node );

        for( auto&& env : normal_envs_ ) {
            env->append_outer_referenced_ast( node );
            env->propagate_outer_referenced_ast( node );
        }

        for( auto&& env : template_envs_ ) {
            env->append_outer_referenced_ast( node );
            env->propagate_outer_referenced_ast( node );
        }

        for( auto&& env : instanced_envs_ ) {
            env->append_outer_referenced_ast( node );
            env->propagate_outer_referenced_ast( node );
        }
    }

} // namespace rill
