//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/identifier_collector.hpp>
#include <rill/semantic_analysis/messaging.hpp>
#include <rill/semantic_analysis/message_code.hpp>

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
        RILL_VISITOR_OP( identifier_collector, ast::module, s, parent_env ) const
        {
            // build environment
            auto const& module_name = make_module_name( base_path_, s );
            auto module_env = g_env_->make_module( module_name, s->fullpath );
            assert( module_env != nullptr );

            module_env->link_with_ast( s );

            dispatch( s->program, module_env );
        }


        // Root Scope
        RILL_VISITOR_OP( identifier_collector, ast::statements, s, parent_env ) const
        {
            for( auto const& ss : s->statements_ )
                dispatch( ss, parent_env );
        }


        RILL_VISITOR_OP( identifier_collector, ast::can_be_template_statement, s, parent_env ) const
        {
        }


        RILL_VISITOR_OP( identifier_collector, ast::block_statement, s, parent_env ) const
        {
            dispatch( s->statements_, parent_env );
        }


        //
        RILL_VISITOR_OP( identifier_collector, ast::expression_statement, s, parent_env ) const
        {
            // DO NOT COLLECT IDENTIFIERS
        }

        //
        RILL_VISITOR_OP( identifier_collector, ast::function_definition_statement, s, parent_env ) const
        {
            // Function symbol that on (global | namespace)

            rill_dout << "IdentifierCollector::Function" << std::endl
                      << " collected          : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << " param_num          : " << s->get_parameter_list().size() << std::endl
                      << " is_template_layout : " << s->is_template_layout() << std::endl;

            if ( s->is_template_layout() ) {
                auto&& multiset_env = cast_to<multiple_set_environment>( parent_env );
                assert( multiset_env != nullptr );
                kind_check( multiset_env, kind::type_value::e_function, s, parent_env );

                multiset_env->set_inner_env_symbol_kind( kind::type_value::e_function );

            } else {
                if ( auto&& e = parent_env->is_exist( s->get_identifier() ) ) {
                    kind_check( *e, kind::type_value::e_function, s, parent_env );
                }

                // add function symbol to current environment
                RILL_PP_TIE(
                    multiset_env, f_env,
                    parent_env->mark_as( kind::k_function, s->get_identifier(), s )
                    );
                multiset_env->add_to_normal_environments( f_env );
            }
        }


        //
        RILL_VISITOR_OP( identifier_collector, ast::variable_declaration_statement, s, parent_env ) const
        {
            // Variable symbol that on (global | namespace)
            // TODO: make variable forward referenceable
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::extern_function_declaration_statement, s, parent_env ) const
        {
            // Function symbol that on (global | namespace)

            rill_dout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            if ( s->is_template_layout() ) {
                semantic_error(
                    message_code::e_different_kind_symbol,
                    s,
                    parent_env,
                    format( "[ice] not implemented" )
                    );

            } else {
                if ( auto&& e = parent_env->is_exist( s->get_identifier() ) ) {
                    kind_check( *e, kind::type_value::e_function, s, parent_env );
                }

                // add function symbol to current environment
                RILL_PP_TIE(
                    set_environment, f_env,
                    parent_env->mark_as( kind::k_function, s->get_identifier(), s )
                    );
                set_environment->add_to_normal_environments( f_env );
            }
        }


        //
        RILL_VISITOR_OP( identifier_collector, ast::extern_class_declaration_statement, s, parent_env ) const
        {
            if ( s->is_template_layout() ) {
                auto&& multiset_env = cast_to<multiple_set_environment>( parent_env );
                assert( multiset_env != nullptr );
                kind_check( multiset_env, kind::type_value::e_class, s, parent_env );

                multiset_env->set_inner_env_symbol_kind( kind::type_value::e_class );

            } else {
                if ( auto&& e = parent_env->is_exist( s->get_identifier() ) ) {
                    kind_check( *e, kind::type_value::e_class, s, parent_env );
                }

                RILL_PP_TIE(
                    set_environment, c_env,
                    parent_env->mark_as( kind::k_class, s->get_identifier(), s )
                    );
                set_environment->add_to_normal_environments( c_env );
            }
        }



        //
        RILL_VISITOR_OP( identifier_collector, ast::class_definition_statement, s, parent_env ) const
        {
            if ( s->is_template_layout() ) {
                auto&& multiset_env = cast_to<multiple_set_environment>( parent_env );
                assert( multiset_env != nullptr );
                kind_check( multiset_env, kind::type_value::e_class, s, parent_env );

                multiset_env->set_inner_env_symbol_kind( kind::type_value::e_class );

            } else {
                if ( auto&& e = parent_env->is_exist( s->get_identifier() ) ) {
                    kind_check( *e, kind::type_value::e_class, s, parent_env );
                }

                // add class symbol to current environment
                RILL_PP_TIE(
                    set_environment, c_env,
                    parent_env->mark_as( kind::k_class, s->get_identifier(), s )
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

            rill_dout << "collected : " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                      << "param_num : " << s->get_parameter_list().size() << std::endl;

            if ( s->is_template_layout() ) {
                semantic_error(
                    message_code::e_different_kind_symbol,
                    s,
                    parent_env,
                    format( "[ice] not implemented" )
                    );

            } else {
                if ( auto&& e = parent_env->is_exist( s->get_identifier() ) ) {
                    kind_check( *e, kind::type_value::e_function, s, parent_env );
                }

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
            if ( auto&& e = parent_env->is_exist( s->get_identifier() ) ) {
                kind_check( *e, kind::type_value::e_variable, s, parent_env );
            }

            // prevent redefinition
            auto const& val_decl = s->declaration_;
            auto const& unit = val_decl.decl_unit;
            if ( auto const& v = parent_env->find_on_env( unit.name ) ) {
                save_stock_message_pivot();

                auto const& val_decl_ast
                    = g_env_->get_related_ast( v->get_id() );

                save_appendix_information(
                    message_code::e_reference,
                    val_decl_ast,
                    v,
                    format( "reference" )
                    );

                semantic_error(
                    message_code::e_variable_is_already_defined,
                    unit.name,
                    parent_env,
                    format( "class variable is already defined" ),
                    true
                    );
            }

            // variable declared in class scope is forward referencable
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


        auto identifier_collector::get_filepath(
            const_environment_base_ptr const& env
            ) const
            -> boost::filesystem::path
        {
            if ( env != nullptr ) {
                auto const& root
                    = env->root_env();
                assert( root != nullptr );
                auto const& module_env
                    = cast_to<module_environment const>( root );

                return module_env->get_filepath();

            } else {
                return "unknown";
            }
        }

        auto identifier_collector::kind_check(
            const_environment_unit_ptr const& env,
            kind::type_value const& kind,
            ast::const_ast_base_ptr const& s,
            environment_base_ptr const& parent_env
            ) const
            -> void
        {
            if ( kind == kind::type_value::e_function
                 || kind == kind::type_value::e_class
                 || kind == kind::type_value::e_variable
                )
            {
                auto const& multiset_env = cast_to<multiple_set_environment>( env );
                assert( multiset_env != nullptr );

                if ( multiset_env->get_representation_kind() != kind::type_value::e_none
                     && multiset_env->get_representation_kind() != kind
                    )
                {
                    semantic_error(
                        message_code::e_different_kind_symbol,
                        s,
                        parent_env,
                        format( "Some symbols that are not %1% kind are already defined in this scope" ) % debug_string( kind )
                        );
                }
            }
        }

    } // namespace semantic_analysis
} // namespace rill
