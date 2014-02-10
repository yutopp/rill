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

#include "analyzer_type.hpp"

#include "../ast/value.hpp"
#include "../compile_time/llvm_engine.hpp"


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

                std::cout << "Find Identifier: " << identifier->get_inner_symbol()->to_native_string() << std::endl;
     
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
     
                    return type_detail_pool.construct(
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
                        return type_detail_pool.construct(
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
                    case kind::type_value::e_function:
                    {
                        return type_detail_pool.construct(
                            (type_id_t)type_id_nontype::e_function,
                            target_env
                        );
                    }
     
                    default:
                        std::cerr << "kind2: " << debug_string( template_set_env->get_inner_env_symbol_kind() ) << std::endl;
                        assert( false && "[[CE]] invalid..." );
                        break;
                    }
                    break;
                }

                // Class identifier should be "type" type
                case kind::type_value::e_class:
                {
    /*                auto const& class_env
                        = std::static_pointer_cast<class_symbol_environment>( target_env );
    */
                    auto const& type_class_env = root_env->lookup( ast::make_identifier( "type" ) );
                    assert( type_class_env != nullptr );  // literal type must exist
                    return type_detail_pool.construct(
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
                return type_detail_pool.construct(
                    type_id_undefined,
                    nullptr
                );
            }
        } // namespace detail



        template<typename Pool, typename EnvPtr>
        auto solve_identifier(
            Pool& type_detail_pool,
            ast::const_identifier_value_ptr const& identifier,
            EnvPtr const& parent_env,
            EnvPtr const& root_env,
            bool const do_not_lookup = false
            ) -> type_detail_ptr
        {
            return detail::generic_solve_identifier( type_detail_pool, identifier, parent_env, root_env, do_not_lookup );
        }


        template<typename Pool, typename EnvPtr>
        auto solve_identifier(
            Pool& type_detail_pool,
            ast::const_template_instance_value_ptr const& identifier,
            EnvPtr const& parent_env,
            EnvPtr const& root_env,
            bool const do_not_lookup = false
            ) -> type_detail_ptr
        {
            auto const& t_detail
                = detail::generic_solve_identifier( type_detail_pool, identifier, parent_env, root_env, do_not_lookup );
            //
            // v. template_argument()

            std::cout << "eval template arguments!!!" << std::endl;

            // evaluate template parameter...
            auto template_args
                = std::make_shared<type_detail::template_arg_type>();

            // TODO: implement
            //identifier->template_argument();


            return type_detail_pool.construct(
                t_detail->type_id,
                t_detail->target_env,
                nullptr,
                template_args
            );
        }



    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP*/
