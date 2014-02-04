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
    // Function environment construction
    // ====

    auto environment_base::mark_as(
        kind::template_tag,
        ast::identifier_value_base_ptr const& name_identifier,
        ast::statement_ptr const& ast
        )
        -> decltype( static_cast<environment_base *>( nullptr )->incomplete_construct( kind::template_tag(), name_identifier ) )
    {
        // construct incomplete environment( template_set & template )
        auto const p = incomplete_construct( kind::template_tag(), name_identifier );
        auto const& template_set_env = p.first;
        auto const& template_env = p.second;

        std::cout << "Marked, %&%& " << template_set_env->get_id() << " : " << template_env->get_id() << std::endl;

        template_env->link_with_ast( ast );

        return p;
    }




    //
    //
    //
    auto single_identifier_environment_base::incomplete_construct(
        kind::template_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> std::pair<
                template_set_environment_ptr,
                template_environment_ptr
           >
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        auto const& template_set_env = [&]() {
            if ( !is_exist_at_template( symbol_name ) ) {
                // make new incomplete env
                auto const& w_env = allocate_env<template_set_environment>();
                template_env_[symbol_name] = w_env;
            }
            auto const& env = template_env_.at( symbol_name );
            assert( env != nullptr );
            assert( env->get_symbol_kind() == kind::type_value::e_template_set );

            return std::static_pointer_cast<template_set_environment>( env );
        }();

        // allocate incomplete funciton environment
        auto const& incomplete_template_env
            = template_set_env->allocate_inner_env();

        return std::make_pair( template_set_env, incomplete_template_env );
    }

#if 0

    //
    //
    //
    auto single_identifier_environment_base::construct(
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
        auto const& return_type_id = parameter_completed_function_env_pointer->make_type_id( return_class_env, return_type_attr );
        parameter_completed_function_env_pointer->complete( return_type_id, symbol_name );

        //
        parameter_env->add_overload( parameter_completed_function_env_pointer );

        return parameter_completed_function_env_pointer;
    }
#endif

} // namespace rill
