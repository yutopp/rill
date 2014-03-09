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
    // Class environment construction
    // ====

    auto environment_base::mark_as(
        kind::class_tag,
        ast::identifier_value_base_ptr const& class_name,
        ast::statement_ptr const& ast
        )
        -> decltype( static_cast<environment_base *>( nullptr )->incomplete_construct( kind::k_class, class_name ) )
    {
        auto const& class_env = incomplete_construct( kind::k_class, class_name );

        std::cout << "%% Marked(class) " << class_env->get_id() << std::endl;

        //
        class_env->link_with_ast( ast );

        //
        return class_env;
    }


    auto single_identifier_environment_base::incomplete_construct(
        kind::class_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> class_symbol_environment_ptr
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // need parameter wrapper environment because function has parameter information
        auto const& c_env = [&]() {
            if ( !is_instanced( symbol_name ) ) {
                // make new incomplete env
                auto const& i_env = allocate_env<class_symbol_environment>();
                nontemplate_env_[symbol_name] = i_env;
            }
            auto const& env = nontemplate_env_.at( symbol_name );
            assert( env != nullptr );
            assert( env->get_symbol_kind() == kind::type_value::e_class );

            return std::static_pointer_cast<class_symbol_environment>( env );
        }();

        return c_env;
    }


    auto single_identifier_environment_base::construct(
        kind::class_tag,
        ast::identifier_value_base_ptr const& class_name,
        ast::statement_ptr const& ast
/*        const_class_symbol_environment_ptr const& class_env,
          attribute::type_attributes const& type_attr*/
        )
        -> class_symbol_environment_ptr
    {
        auto const& c_env = mark_as( kind::k_class, class_name, ast );

        auto const& symbol_name = class_name->get_inner_symbol()->to_native_string();

        c_env->complete( symbol_name, symbol_name/*qualified name*//*, attribute*/ );

        return c_env;
    }

} // namespace rill
