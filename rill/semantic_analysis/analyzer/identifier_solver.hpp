//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP
#define RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP

#include <memory>

#include "../../environment/environment_kind.hpp"
#include "../../ast/value.hpp"


#include "function_solver.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        namespace detail
        {
            template<typename Pool, typename T, typename EnvPtr>
            auto generic_solve_identifier(
                Pool& type_detail_pool,
                T const& identifier,
                EnvPtr const& parent_env,
                EnvPtr const& root_env,
                bool const do_not_lookup = false
                ) -> type_detail_ptr
            {
                // TODO: check that identifier is from root

                // TODO: fix
                auto const do_lookup = !do_not_lookup;

                auto const found_env
                    = do_lookup
                    ? parent_env->lookup( identifier )
                    : parent_env->find_on_env( identifier )
                    ;
                if ( found_env == nullptr ) {
                    // compilation error
                    assert( false && "[[CE]] identifier was not found..." );
                }

                // debug
                std::cout << "## Finding Identifier: " << identifier->get_inner_symbol()->to_native_string() << std::endl
                          << "## astid: " << identifier->get_id() << std::endl
                          << "## kind: " << debug_string( found_env->get_symbol_kind() ) << std::endl
                          << (const_environment_base_ptr)parent_env << std::endl;


                switch( found_env->get_symbol_kind() ) {
                case kind::type_value::e_multi_set:
                {
                    auto const& set_env
                        = std::static_pointer_cast<multiple_set_environment>( found_env );

                    //
                    switch( set_env->get_representation_kind() ) {
                    case kind::type_value::e_function:
                    {
                        // funcion can be oveloaded, so do not link with identifier
                        return type_detail_pool->construct(
                            (type_id_t)type_id_nontype::e_function,
                            set_env
                            );
                    }

                    case kind::type_value::e_class:
                    {
                        // Class identifier should be "type" type

                        // TODO: completion the incomplete class

                        // class can not be overloaded, so only one symbol will exist in "set environment".
                        auto const& ne = set_env->get_normal_environments();
                        assert( ne.size() == 1 );

                        auto const& class_env
                            = cast_to<class_symbol_environment>( ne.at( 0 ) );
                        assert( class_env != nullptr );

                        std::cout << "()memoed.class " << class_env->get_qualified_name() << std::endl;
                        // link with given identifier!
                        class_env->connect_from_ast( identifier );

                        // TODO: cache this values
                        auto const& type_class_env = [&](){
                            auto const& generic_type_class_set_env
                                = root_env->lookup( ast::make_identifier( "type" ) );
                            assert( generic_type_class_set_env != nullptr );  // literal type must exist
                            auto const& type_class_set_env
                                = cast_to<multiple_set_environment>( generic_type_class_set_env );

                            assert( type_class_set_env != nullptr );

                            auto const& ne
                                = type_class_set_env->get_normal_environments();
                            assert( ne.size() == 1 );

                            return cast_to<class_symbol_environment>( ne.at( 0 ) );
                        }();
                        assert( type_class_env != nullptr );

                        auto const& type_type_id
                            = type_class_env->make_type_id( type_class_env, determine_type_attributes() );

                        return type_detail_pool->construct(
                            type_type_id,
                            type_class_env
                            );
                    }

                    default:
                        std::cerr << "kind: " << debug_string( set_env->get_representation_kind() ) << std::endl;
                        assert( false && "[[CE]] invalid..." );
                        break;
                    }
                    break;

                    assert( false );
                }


                case kind::type_value::e_variable:
                {
                    auto const& variable_env
                        = std::static_pointer_cast<variable_symbol_environment>( found_env );

                    // memoize
                    std::cout << "()memoed.variable" << std::endl;
                    variable_env->connect_from_ast( identifier );

                    // class
                    // variable_env->get_type_id();

                    return type_detail_pool->construct(
                        variable_env->get_type_id(),
                        variable_env
                    );
                }


#if 0
                case kind::type_value::e_template_set:
                {
                    std::cout << "TEMPLATE SET" << std::endl;
                    auto const& template_set_env
                        = std::static_pointer_cast<template_set_environment>( found_env );

                    switch( template_set_env->get_inner_env_symbol_kind() ) {
                    case kind::type_value::e_class:
                    {
                        // DO NOT FORGET TO CONNECT to the instances type from ast


                        // Class identifier should be "type" type...?
                        // COUTION: type_class_env will be multiple_set
                        auto const& type_class_env = root_env->lookup( ast::make_identifier( "type" ) );
                        assert( type_class_env != nullptr );  // literal type must exist

                        auto const& ty_id = type_class_env->make_type_id(
                            type_class_env,
                            determine_type_attributes()
                            );

                        return type_detail_pool->construct(
                            (type_id_t)type_id_nontype::e_template_class,
                            found_env,
                            nullptr/*not nested*/,
                            std::make_shared<type_detail::template_arg_type>()
                            );
                    }

                    case kind::type_value::e_function:
                    {
                        return type_detail_pool->construct(
                            (type_id_t)type_id_nontype::e_function,
                            found_env,
                            nullptr/*not nested*/,
                            std::make_shared<type_detail::template_arg_type>()
                            );
                    }

                    default:
                        std::cerr << "kind: " << debug_string( template_set_env->get_inner_env_symbol_kind() ) << std::endl;
                        assert( false && "[[error]] this template set was not supported yet..." );
                        break;
                    }
                    break;
                }
#endif



                default:
                    std::cerr << "kind: " << debug_string( found_env->get_symbol_kind() ) << std::endl;
                    assert( false && "[[CE]] invalid..." );
                    break;
                }

                assert( false );
                return type_detail_pool->construct(
                    type_id_undefined,
                    nullptr
                );
            }
        } // namespace detail



        // for Identifier
        template<typename AnalyzerPtr>
        auto solve_identifier(
            AnalyzerPtr const& a,
            ast::const_identifier_value_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_not_lookup
            ) -> type_detail_ptr
        {
            return detail::generic_solve_identifier(
                a->type_detail_pool_,
                identifier,
                parent_env,
                a->root_env_,
                do_not_lookup
                );
        }

        template<typename AnalyzerPtr>
        inline auto solve_identifier(
            AnalyzerPtr const& a,
            ast::const_identifier_value_ptr const& identifier,
            environment_base_ptr const& parent_env
            ) -> type_detail_ptr
        {
            return solve_identifier( a, identifier, parent_env, false );
        }


        // for Template Instance Identifier
        template<typename AnalyzerPtr>
        auto solve_identifier(
            AnalyzerPtr const& a,
            ast::const_template_instance_value_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_not_lookup
            ) -> type_detail_ptr
        {
            auto const ty_detail
                = detail::generic_solve_identifier(
                    a->type_detail_pool_,
                    identifier,
                    parent_env,
                    a->root_env_,
                    do_not_lookup
                    );
            //
            // v. template_argument()

            std::cout << "eval template arguments!!!" << std::endl;

            // evaluate template arguments
            type_detail::template_arg_type template_args
                = a->evaluate_template_args(
                    identifier->template_argument(),
                    parent_env
                    );

            // set evaluated template args
            assert( ty_detail->template_args != nullptr );
            (*ty_detail->template_args) = std::move( template_args );

            // class template instantiation!!!
            if ( is_nontype_id( ty_detail->type_id ) ) {
                if ( ty_detail->type_id == (type_id_t)type_id_nontype::e_template_class ) {
                    // returns instances class symbol environment
                    auto const i_c_env
                        = a->instanciate_class(
                            ty_detail,
                            parent_env
                            );

                    // !! important
                    // memoize
                    std::cout << "()memoed.template_class" << std::endl;
                    i_c_env->connect_from_ast( identifier );
                }
            }

            return ty_detail;
        }

        template<typename AnalyzerPtr>
        inline auto solve_identifier(
            AnalyzerPtr const& a,
            ast::const_template_instance_value_ptr const& template_identifier,
            environment_base_ptr const& parent_env
            ) -> type_detail_ptr
        {
            return solve_identifier( a, template_identifier, parent_env, false );
        }






        // TODO: throw the exception, when failed to solve the type symbol that contained in the expression
        template<typename AnalyzerPtr, typename F>
        auto solve_type(
            AnalyzerPtr const& a,
            ast::type_expression_ptr const& type_expression,
            environment_base_ptr const& parent_env,
            F&& callback
            ) -> type_detail_ptr
        {
            std::cout << "ABA" << std::endl;

            auto const ty_detail = a->eval_type_expression_as_ctfe( type_expression, parent_env );
            auto const& ty_id = ty_detail->type_id;
            std::cout << "ABABA" << std::endl;

            auto const ty = parent_env->get_type_at( ty_id );  // copy Ty...

            // TODO: qualify by const( class_symbol_environment => class_symbol_environment const )
            auto const& class_env
                = std::static_pointer_cast<class_symbol_environment>(
                    parent_env->get_env_strong_at( ty.class_env_id )
                    );
            assert( class_env != nullptr );
            assert( class_env->get_symbol_kind() == kind::type_value::e_class );
            std::cout << "ABABA2" << std::endl;

            callback( ty_detail, ty, class_env );

            return ty_detail;
        }



    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP*/
