//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/identifier_collector.hpp>
#include <rill/environment/environment.hpp>
#include <rill/environment/global_environment.hpp>
#include <rill/environment/make_module_name.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>

#include <rill/utility/tie.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        identifier_collector::identifier_collector(
            global_environment_ptr const& g_env,
            boost::filesystem::path const& base_path
            )
            : g_env_( g_env )
            , base_path_( base_path )
        {}

        // Root Scope
        RILL_VISITOR_OP( identifier_collector, ast::module, s, env ) const
        {
            // build environment
            auto const& module_name = make_module_name( base_path_, s );
            auto module_env = g_env_->make_module( module_name );
            assert( module_env != nullptr );

            dispatch( s->program, module_env );
        }


        // Root Scope
        RILL_VISITOR_OP( identifier_collector, ast::statements, s, env ) const
        {
            for( auto const& ss : s->statements_ )
                dispatch( ss, env );
        }


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

            std::cout << "IdentifierCollector::Function" << std::endl
                      << " collected          : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << " param_num          : " << s->get_parameter_list().size() << std::endl
                      << " is_template_layout : " << s->is_template_layout() << std::endl;

            if ( s->is_template_layout() ) {
                auto&& multiset_env = cast_to<multiple_set_environment>( env );
                assert( multiset_env != nullptr );

                if ( multiset_env->get_representation_kind() != kind::type_value::e_none
                     && multiset_env->get_representation_kind() != kind::type_value::e_function
                    ) {
                    assert( false && "Some symbols that are not function_type are already defined in this scope" );
                }

                multiset_env->set_inner_env_symbol_kind( kind::type_value::e_function );

            } else {
                // add function symbol to current environment
                RILL_PP_TIE(
                    multiset_env, f_env,
                    env->mark_as( kind::k_function, s->get_identifier(), s )
                    );
                multiset_env->add_to_normal_environments( f_env );
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

            if ( s->is_template_layout() ) {
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
        RILL_VISITOR_OP( identifier_collector, ast::extern_class_declaration_statement, s, env ) const
        {
            if ( s->is_template_layout() ) {
                auto&& multiset_env = cast_to<multiple_set_environment>( env );
                assert( multiset_env != nullptr );

                if ( multiset_env->get_representation_kind() != kind::type_value::e_none
                     && multiset_env->get_representation_kind() != kind::type_value::e_class
                    ) {
                    assert( false && "Some symbols that are not class_type are already defined in this scope" );
                }

                multiset_env->set_inner_env_symbol_kind( kind::type_value::e_class );

            } else {
                RILL_PP_TIE(
                    set_environment, c_env,
                    env->mark_as( kind::k_class, s->get_identifier(), s )
                    );
                set_environment->add_to_normal_environments( c_env );
            }
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_definition_statement, s, env ) const
        {
            if ( s->is_template_layout() ) {
                auto&& multiset_env = cast_to<multiple_set_environment>( env );
                assert( multiset_env != nullptr );

                if ( multiset_env->get_representation_kind() != kind::type_value::e_none
                     && multiset_env->get_representation_kind() != kind::type_value::e_class
                    ) {
                    assert( false && "Some symbols that are not class_type are already defined in this scope" );
                }

                multiset_env->set_inner_env_symbol_kind( kind::type_value::e_class );

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



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_function_definition_statement, s, parent_env ) const
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // TODO: add support for template

            std::cout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            if ( s->is_template_layout() ) {
                assert( false && "[ice] not implemented" );

            } else {
                // add function symbol to current environment
                RILL_PP_TIE(
                    multiset_env, f_env,
                    parent_env->mark_as( kind::k_function, s->get_identifier(), s )
                    );
                f_env->set_parent_class_env_id( parent_env->get_id() );

                multiset_env->add_to_normal_environments( f_env );
            }
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_variable_declaration_statement, s, parent_env ) const
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // prevent redefinition
            auto const& val_decl = s->declaration_;
            auto const& unit = val_decl.decl_unit;
            if ( auto const& v = parent_env->find_on_env( unit.name ) ) {
                assert( false && "[[error]] variable is already defined" );
            }

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
            s->get_inner_statement()->mark_as_template_layout();

            // make template envitonment with linking to this AST node
            RILL_PP_TIE( multiset_env, template_env, parent_env->mark_as( kind::k_template, s->get_identifier(), s ) );

            //
            multiset_env->add_to_template_environments( template_env );

            // delegate inner statement...
            // checking weather the inner statement is templated is done in per collector
            dispatch( s->get_inner_statement(), multiset_env );
        }

    } // namespace semantic_analysis
} // namespace rill
