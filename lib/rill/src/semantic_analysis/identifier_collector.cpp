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

#include <rill/utility/tie.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // Root Scope
        RILL_VISITOR_OP( identifier_collector, ast::statements, s, env ) const
        {
            for( auto const& ss : s->statements_ )
                dispatch( ss, env );
        }


        // Root Scope
        RILL_VISITOR_OP( identifier_collector, ast::can_be_template_statement, s, env ) const
        {
        }


        RILL_VISITOR_OP( identifier_collector, ast::block_statement, s, env ) const
        {
            dispatch( s->statements_, env );
        }


        //
        RILL_VISITOR_OP( identifier_collector, ast::expression_statement, s, env ) const
        {
            // DO NOT COLLECT IDENTIFIERS
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::function_definition_statement, s, env ) const
        {
            // Function symbol that on (global | namespace)

            std::cout << "collected    : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num    : " << s->get_parameter_list().size() << std::endl
                      << "is_templated : " << s->is_templated() << std::endl;

            if ( s->is_templated() ) {
                // TODO: add symbol type duplicate check
                auto&& set_env = cast_to<multiple_set_environment>( env );
                assert( set_env != nullptr );

                set_env->set_inner_env_symbol_kind( kind::type_value::e_function );

            } else {
                // add function symbol to current environment
                RILL_PP_TIE(
                    set_environment, f_env,
                    env->mark_as( kind::k_function, s->get_identifier(), s )
                    );
                set_environment->add_to_normal_environments( f_env );
            }
        }


        //
        RILL_VISITOR_OP( identifier_collector, ast::variable_declaration_statement, s, env ) const
        {
            // Variable symbol that on (global | namespace)
            // TODO: make variable forward referenceable
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::extern_function_declaration_statement, s, env ) const
        {
            // Function symbol that on (global | namespace)

            std::cout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            if ( s->is_templated() ) {
                assert( false && "[ice] not implemented" );

            } else {
                // add function symbol to current environment
                RILL_PP_TIE(
                    set_environment, f_env,
                    env->mark_as( kind::k_function, s->get_identifier(), s )
                    );
                set_environment->add_to_normal_environments( f_env );
            }
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_definition_statement, s, env ) const
        {
            // Class symbol that on (global | namespace)

            if ( s->is_templated() ) {
                // TODO: add symbol type duplicate check
                /*std::static_pointer_cast<template_set_environment>( env )->set_inner_env_symbol_kind(
                    kind::type_value::e_class
                    );*/

            } else {
                if ( is_builtin() ) {
                    // for builtin symbol
                    env->construct( kind::k_class, s->get_identifier(), s );

                } else {
                    // add class symbol to current environment
                    RILL_PP_TIE(
                        set_environment, c_env,
                        env->mark_as( kind::k_class, s->get_identifier(), s )
                        );
                    set_environment->add_to_normal_environments( c_env );

                    // build inner environment
                    dispatch( s->inner_, c_env );
                }
            }
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_function_definition_statement, s, parent_env ) const
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // TODO: add support for template

            std::cout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            if ( s->is_templated() ) {
                assert( false && "[ice] not implemented" );

            } else {
                // add function symbol to current environment
                auto const& f_env_pair = parent_env->mark_as( kind::k_function, s->get_identifier(), s );
                f_env_pair.second->set_parent_class_env_id( parent_env->get_id() );
            }
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_variable_declaration_statement, s, parent_env ) const
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // variable declared in class scope should be forward referencable

            // add variable symbol to current environment
            auto const& v_env
                = parent_env->mark_as( kind::k_variable, s->get_identifier(), s );
            v_env->set_parent_class_env_id( parent_env->get_id() );
        }


        //
        RILL_VISITOR_OP( identifier_collector, ast::template_statement, s, parent_env ) const
        {
            // mark inner AST node as templated
            s->get_inner_statement()->mark_as_template();

            // make template envitonment with linking to this AST node
            RILL_PP_TIE( set_env, template_env, parent_env->mark_as( kind::k_template, s->get_identifier(), s ) );

            //
            set_env->add_to_template_environments( template_env );

            // delegate inner statement...
            dispatch( s->get_inner_statement(), set_env );
        }

    } // namespace semantic_analysis
} // namespace rill
