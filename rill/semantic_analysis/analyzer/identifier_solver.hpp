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
#include "../../utility/tie.hpp"

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
                // TODO: check is identifier from root

                std::cout << "Finding Identifier: "
                          << identifier->get_inner_symbol()->to_native_string() << std::endl
                          << "astptr: " << identifier->get_id() << std::endl
                          << (const_environment_base_ptr)parent_env << std::endl;;

                auto const target_env
                    = do_not_lookup
                    ? parent_env->find_on_env( identifier )
                    : parent_env->lookup( identifier )
                    ;
                if ( target_env == nullptr ) {
                    // compilation error
                    assert( false && "[[CE]] identifier was not found..." );
                }

                switch( target_env->get_symbol_kind() ) {
                case kind::type_value::e_variable:
                {
                    auto const& variable_env
                        = std::static_pointer_cast<variable_symbol_environment>( target_env );

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

                case kind::type_value::e_parameter_wrapper:
                {
                    auto const& has_parameter_env
                        = std::static_pointer_cast<has_parameter_environment_base>( target_env );

                    switch( has_parameter_env->get_inner_symbol_kind() ) {
                    case kind::type_value::e_function:
                    {
                        return type_detail_pool->construct(
                            (type_id_t)type_id_nontype::e_function,
                            target_env
                        );
                    }

                    default:
                        std::cerr << "kind: " << debug_string( has_parameter_env->get_inner_symbol_kind() ) << std::endl;
                        assert( false && "[[CE]] invalid..." );
                        break;
                    }
                    break;
                }

                case kind::type_value::e_template_set:
                {
                    std::cout << "TEMPLATE SET" << std::endl;
                    auto const& template_set_env
                        = std::static_pointer_cast<template_set_environment>( target_env );

                    switch( template_set_env->get_inner_env_symbol_kind() ) {
                    case kind::type_value::e_class:
                    {
                        // DO NOT FORGET TO CONNECT to the instances type from ast


                        // Class identifier should be "type" type...?
                        auto const& type_class_env = root_env->lookup( ast::make_identifier( "type" ) );
                        assert( type_class_env != nullptr );  // literal type must exist

                        auto const& ty_id = type_class_env->make_type_id(
                            type_class_env,
                            determine_type_attributes()
                            );

                        return type_detail_pool->construct(
                            (type_id_t)type_id_nontype::e_template_class,
                            target_env,
                            nullptr/*not nested*/,
                            std::make_shared<type_detail::template_arg_type>()
                            );
                    }

                    case kind::type_value::e_function:
                    {
                        return type_detail_pool->construct(
                            (type_id_t)type_id_nontype::e_function,
                            target_env,
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

                // Class identifier should be "type" type
                case kind::type_value::e_class:
                {
                    // TODO: completion the incomplete class

                    auto const& class_env
                        = std::static_pointer_cast<class_symbol_environment>( target_env );

                    // memoize
                    std::cout << "()memoed.class " << class_env->get_qualified_name() << std::endl;
                    class_env->connect_from_ast( identifier );


                    auto const& type_class_env = root_env->lookup( ast::make_identifier( "type" ) );
                    assert( type_class_env != nullptr );  // literal type must exist
                    return type_detail_pool->construct(
                        type_class_env->make_type_id( type_class_env, determine_type_attributes() ),
                        type_class_env
                    );
                }

                default:
                    std::cerr << "kind: " << debug_string( target_env->get_symbol_kind() ) << std::endl;
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
        template<typename AnalyzerPtr, typename EnvPtr>
        auto solve_identifier(
            AnalyzerPtr const& a,
            ast::const_identifier_value_ptr const& identifier,
            EnvPtr const& parent_env,
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


        // for Template Instance Identifier
        template<typename AnalyzerPtr, typename EnvPtr>
        auto solve_identifier(
            AnalyzerPtr const& a,
            ast::const_template_instance_value_ptr const& identifier,
            EnvPtr const& parent_env,
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


        // returns "environment of expression's TYPE", "evaled value"
        template<typename AnalyzerPtr, typename EnvPtr>
        auto eval_expression_as_ctfe(
            AnalyzerPtr const& a,
            ast::expression_ptr const& expression,
            EnvPtr const& parent_env
            ) -> std::tuple<
                const_class_symbol_environment_ptr,
                void*
            >
        {
            std::cout << "TYPE expresison!!!!!!" << std::endl;

            // solve semantics
            auto const& ty_detail = a->dispatch( expression, parent_env );

            //
            if ( is_nontype_id( ty_detail->type_id ) ) {
                assert( false && "[ice]" );
            }

            // get environment of the type expresison
            auto const& ty
                = a->root_env_->get_type_at( ty_detail->type_id );

            auto c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    a->root_env_->get_env_strong_at( ty.class_env_id )
                    );
            assert( c_env != nullptr );

            // eval expression of arguments
            auto evaled_value = a->ctfe_engine_->execute( expression, parent_env );
            assert( evaled_value != nullptr );

            return std::forward_as_tuple(
                std::move( c_env ),         // type of eveled_value
                std::move( evaled_value )   //
                );
        }




        // for Template Instance Identifier
        // guarantees the return type is TYPE
        template<typename AnalyzerPtr, typename EnvPtr>
        auto ctfe_as_type(
            AnalyzerPtr const& a,
            ast::type_expression_ptr const& type_expression,
            EnvPtr const& parent_env
            ) -> type_detail_ptr
        {
            std::cout << "TYPE expresison!!!!!!" << std::endl;
            RILL_PP_TIE(
                c_env, evaled_value,
                eval_expression_as_ctfe( a, type_expression, parent_env )
                );

            //
            if ( c_env->get_base_name() != "type" ) {
                assert( false && "[[ice]] value parameter was not supported yet" );
            }

            return static_cast<type_detail_ptr>( evaled_value );
        }


        // TODO: throw the exception, when failed to solve the type symbol that contained in the expression
        template<typename AnalyzerPtr, typename EnvPtr, typename F>
        auto solve_type(
            AnalyzerPtr const& a,
            ast::type_expression_ptr const& type_expression,
            EnvPtr const& parent_env,
            F&& callback
            ) -> type_detail_ptr
        {
            std::cout << "ABA" << std::endl;

            auto const ty_detail = ctfe_as_type( a, type_expression, parent_env );
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
