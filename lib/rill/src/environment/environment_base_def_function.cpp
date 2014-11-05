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

#include <rill/semantic_analysis/analyzer.hpp>
#include <rill/utility/tie.hpp>


namespace rill
{
    // ====
    // Function environment construction
    // ====

    auto environment_base::mark_as(
        kind::function_tag,
        ast::identifier_value_base_ptr const& name_identifier,
        ast::statement_ptr const& ast
        )
        -> std::pair<multiple_set_environment_ptr, function_symbol_environment_ptr>
    {
        // construct incomplete environment( parameter wrapper & function )
        RILL_PP_TIE( set_env, created_function_env, incomplete_construct( kind::function_tag(), name_identifier ) );

        std::cout << "Marked, %&%& " << set_env->get_id() << " : " << created_function_env->get_id() << std::endl;

        //
        created_function_env->link_with_ast( ast );

        return std::make_pair(
            std::move( set_env ),
            std::move( created_function_env )
            );
    }


    //
    // construct incomplete function environment with preparerion
    //
    auto environment_base::incomplete_construct(
        kind::function_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> std::pair<multiple_set_environment_ptr, function_symbol_environment_ptr>
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // wrapper environment
        auto const& set_environment
            = b_.lock()->allocate_env_unless_exist<multiple_set_environment>(
                shared_from_this(),
                symbol_name,
                symbol_name
                );

        // set kind
        assert( set_environment->get_representation_kind() == kind::type_value::e_none
                || set_environment->get_representation_kind() == kind::type_value::e_function
            );
        set_environment->set_inner_env_symbol_kind( kind::type_value::e_function );

        // allocate incomplete funciton environment
        auto const& incomplete_function_env
            = set_environment->allocate_inner_env<function_symbol_environment>(
                symbol_name
                );

        return std::make_pair(
            std::move( set_environment ),
            std::move( incomplete_function_env )
            );
    }


    //
    auto environment_base::construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const& function_name,
            ast::statement_ptr const& ast,
            function_env_generator_scope_type const& parameter_decl_initializer,
            class_symbol_environment_ptr const& return_class_env,
            attribute::type_attributes const& return_type_attr
            ) -> function_symbol_environment_ptr
    {
#if 0
        auto const& p_i_pair = mark_as( kind::k_function, function_name, ast );
        auto const& set_environment = p_i_pair.first;
        auto const& incomplete_function_env = p_i_pair.second;

        // complete parameter decl
        auto const& parameter_completed_function_env_pointer = parameter_decl_initializer( incomplete_function_env );

        // complete return type, name
        auto const& return_type_id = b_.lock()->make_type_id( return_class_env, return_type_attr );
        parameter_completed_function_env_pointer->decide_return_type( return_type_id );

        //
        parameter_completed_function_env_pointer->change_progress_to_checked();

        //
        auto const& mangled_name = semantic_analysis::make_mangled_name( parameter_completed_function_env_pointer );
        parameter_completed_function_env_pointer->complete( mangled_name );

        //
        set_environment->add_to_normal_environments( parameter_completed_function_env_pointer );

        return parameter_completed_function_env_pointer;
#endif
        assert( false );
        return nullptr;
    }

} // namespace rill
