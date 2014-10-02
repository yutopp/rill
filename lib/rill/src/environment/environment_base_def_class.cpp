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

#include <rill/utility/tie.hpp>


namespace rill
{
    // ====
    // Class environment construction
    // ====

    auto environment_base::mark_as(
        kind::class_tag,
        ast::identifier_value_base_ptr const& class_name,
        ast::statement_ptr const& ast
        )
        -> std::pair<multiple_set_environment_ptr, class_symbol_environment_ptr>
    {
        RILL_PP_TIE(
            set_environment, class_env,
            incomplete_construct( kind::k_class, class_name )
            );

        std::cout << "%% Marked(class) " << class_env->get_id() << std::endl;

        //
        class_env->link_with_ast( ast );

        //
        return std::make_pair(
            std::move( set_environment ),
            std::move( class_env )
            );
    }


    auto environment_base::incomplete_construct(
        kind::class_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> std::pair<multiple_set_environment_ptr, class_symbol_environment_ptr>
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // wrapper environment
        auto const& set_environment
            = allocate_env_unless_exist<multiple_set_environment>( symbol_name, symbol_name );

        // allocate incomplete funciton environment
        auto const& incomplete_class_env
            = set_environment->allocate_inner_env<class_symbol_environment>(
                symbol_name
                );

        // set kind
        assert( set_environment->get_representation_kind() == kind::type_value::e_none
                || set_environment->get_representation_kind() == kind::type_value::e_class
            );
        set_environment->set_inner_env_symbol_kind( kind::type_value::e_class );

        return std::make_pair(
            std::move( set_environment ),
            std::move( incomplete_class_env )
            );
    }


    // normal construction
    auto environment_base::construct(
        kind::class_tag,
        ast::identifier_value_base_ptr const& class_name,
        ast::statement_ptr const& ast
/*        const_class_symbol_environment_ptr const& class_env,
          attribute::type_attributes const& type_attr*/
        )
        -> class_symbol_environment_ptr
    {
        RILL_PP_TIE(
            set_environment, c_env,
            mark_as( kind::k_class, class_name, ast )
            );

        auto const& symbol_name = class_name->get_inner_symbol()->to_native_string();

        c_env->complete( symbol_name, symbol_name/*qualified name*//*, attribute*/ );

        set_environment->add_to_normal_environments( c_env );

        return c_env;
    }

} // namespace rill
