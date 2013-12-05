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
        kind::function_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& name_identifier,
        ast::statement_ptr const& ast
        )
        -> decltype( static_cast<environment_base *>( nullptr )->incomplete_construct( kind::function_tag(), name_identifier ) )
    {
        // construct incomplete environment( parameter wrapper & function )
        auto const p = incomplete_construct( kind::function_tag(), name_identifier );
        auto const& has_param_env = p.first;
        auto const& created_function_env = p.second;

        std::cout << "Marked, %&%& " << has_param_env->get_id() << " : " << created_function_env->get_id() << std::endl;

        //
        root_shared_resource_->env_id_to_ast_map.add( has_param_env->get_id(), ast );           // 
        root_shared_resource_->env_id_to_ast_map.add( created_function_env->get_id(), ast );    // related environment of created_function_env is parent envitroment of it

        //
        root_shared_resource_->ast_to_env_id_map.add( ast, created_function_env->get_id() );

        return p;
    }


    auto single_identifier_environment_base::incomplete_construct(
        kind::function_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& name
        )
        -> std::pair<
                std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                function_symbol_environment_ptr
           >
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // need parameter wrapper environment because function has parameter information
        auto const& parameter_env = [&]() {
            if ( !is_instanced( symbol_name ) ) {
                // make new incomplete env
                auto const& w_env = allocate_env<has_parameter_environment<function_symbol_environment>>( shared_from_this() );
                instanced_env_[symbol_name] = w_env;
            }
            auto const& env = instanced_env_.at( symbol_name );
            assert( env != nullptr );
            assert( env->get_symbol_kind() == kind::type_value::e_parameter_wrapper );
            assert( std::dynamic_pointer_cast<has_parameter_environment_base>( env )->get_inner_symbol_kind() == kind::type_value::e_function );

            return std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment>>( env );
        }();

        // allocate incomplete funciton environment
        auto const& incomplete_function_env = parameter_env->allocate_inner_env_as_incomplete();

        return std::make_pair( parameter_env, incomplete_function_env );
    }


    auto single_identifier_environment_base::construct(
            kind::function_tag,
            ast::intrinsic::single_identifier_value_base_ptr const& function_name,
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

} // namespace rill
