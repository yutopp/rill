//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/identifier_collector.hpp>
#include <rill/environment/environment.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // Root Scope
        RILL_TV_OP( identifier_collector, ast::statements, s, env )
        {
            for( auto const& ss : s->statement_list_ )
                dispatch( ss, env );
        }


        // Root Scope
        RILL_TV_OP( identifier_collector, ast::can_be_template_statement, s, env )
        {
        }


        RILL_TV_OP( identifier_collector, ast::block_statement, s, env )
        {
            dispatch( s->statements_, env );
        }


        //
        RILL_TV_OP( identifier_collector, ast::expression_statement, s, env )
        {
            // DO NOT COLLECT IDENTIFIERS
        }



        //
        RILL_TV_OP( identifier_collector, ast::function_definition_statement, s, env )
        {
            // Function symbol that on (global | namespace)

            std::cout << "collected    : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num    : " << s->get_parameter_list().size() << std::endl
                      << "is_templated : " << s->is_templated() << std::endl;

            if ( s->is_templated() ) {
                // TODO: add symbol type duplicate check
                std::static_pointer_cast<template_set_environment>( env )->set_inner_env_symbol_kind(
                    kind::type_value::e_function
                    );

            } else {
                // add function symbol to current environment
                env->mark_as( kind::k_function, s->get_identifier(), s );
            }
        }


        //
        RILL_TV_OP( identifier_collector, ast::variable_declaration_statement, s, env )
        {
            // Variable symbol that on (global | namespace)
            // TODO: make variable forward referenceable
        }



        //
        RILL_TV_OP( identifier_collector, ast::extern_function_declaration_statement, s, env )
        {
            // Function symbol that on (global | namespace)

            std::cout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            if ( s->is_templated() ) {
                assert( false );

            } else {
                // add function symbol to current environment
                env->mark_as( kind::k_function, s->get_identifier(), s );
            }
        }



        //
        RILL_TV_OP( identifier_collector, ast::class_definition_statement, s, env )
        {
            // Class symbol that on (global | namespace)

            if ( s->is_templated() ) {
                assert( false );

            } else {
                // add class symbol to current environment
                auto c_env = env->mark_as( kind::k_class, s->get_identifier(), s );

                // build environment
                dispatch( s->inner_, c_env );
            }
        }



        //
        RILL_TV_OP( identifier_collector, ast::class_function_definition_statement, s, parent_env )
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // TODO: add support for template

            std::cout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            // add function symbol to current environment
            auto const& f_env_pair = parent_env->mark_as( kind::k_function, s->get_identifier(), s );
            f_env_pair.second->set_parent_class_env_id( parent_env->get_id() );
        }



        //
        RILL_TV_OP( identifier_collector, ast::class_variable_declaration_statement, s, parent_env )
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // variable declared in class scope should be forward referencable

            // add variable symbol to current environment
            auto const& v_env
                = parent_env->mark_as( kind::k_variable, s->get_identifier(), s );
            v_env->set_parent_class_env_id( parent_env->get_id() );
        }


        //
        RILL_TV_OP( identifier_collector, ast::template_statement, s, parent_env )
        {
            // mark AST as templated
            s->get_inner_statement()->mark_as_template();

            // 
            auto const& template_env_pair
                = parent_env->mark_as( kind::k_template, s->get_identifier(), s );

            // unlike the has_parameter_environment, template_set cannot determine the inner environment type of template
            auto& template_set_env = template_env_pair.first;
            auto& template_env = template_env_pair.second;

            // TODO: link template variables to this env
            //template_env

            // save with argument length
            // if Variaic templates, use template_environment::variadic_length
            // TODO: support variadic templates
            // FIXME: currently preset as 1 length
            template_set_env->add_candidate( template_env );

            // delegate inner statement...
            dispatch( s->get_inner_statement(), template_set_env );
        }


    } // namespace semantic_analysis
} // namespace rill
