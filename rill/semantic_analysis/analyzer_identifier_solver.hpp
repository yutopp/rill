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
            template<typename T, typename EnvPtr>
            auto generic_solve_identifier(
                T const& identifier,
                EnvPtr const& parent_env,
                EnvPtr const& root_env,
                bool const is_template,
                bool const do_not_lookup = false
                ) -> type_id_with_env
            {
                // TODO: check is identifier from root

                std::cout << "Find Identifier: " << identifier->get_inner_symbol()->to_native_string() << std::endl;
     
                auto const target_env
                    = is_template
                    ? [&]()
                    {
                        return nullptr;
                    }()
                    : [&]()
                    {
                        return do_not_lookup
                                ? parent_env->find_on_env( identifier )
                                : parent_env->lookup( identifier );
                    }()
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
     
                    return {
                        variable_env->get_type_id(),
                        variable_env
                    };
                }
                    
                case kind::type_value::e_parameter_wrapper:
                {
                    auto const& has_parameter_env
                        = std::static_pointer_cast<has_parameter_environment_base>( target_env );
     
                    switch( has_parameter_env->get_inner_symbol_kind() ) {
                    case kind::type_value::e_function:
                    {
                        return {
                            type_id_special,
                            target_env
                        };
                    }
     
                    default:
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
                    return {
                        type_class_env->make_type_id( type_class_env, determine_type_attributes() ),
                        type_class_env
                    };
                }
     
                default:
                    assert( false && "[[CE]] invalid..." );
                    break;
                }
     
                assert( false );
                return {
                    type_id_undefined,
                    nullptr
                };
            }
        } // namespace detail



        template<typename EnvPtr>
        auto solve_identifier(
            ast::const_identifier_value_ptr const& identifier,
            EnvPtr const& parent_env,
            EnvPtr const& root_env,
            bool const do_not_lookup = false
            ) -> type_id_with_env
        {
            return detail::generic_solve_identifier( identifier, parent_env, root_env, false, do_not_lookup );
        }


        template<typename EnvPtr>
        auto solve_identifier(
            ast::const_template_instance_value_ptr const& identifier,
            EnvPtr const& parent_env,
            EnvPtr const& root_env,
            bool const do_not_lookup = false
            ) -> type_id_with_env
        {
            auto const& id_detail
                = detail::generic_solve_identifier( identifier, parent_env, root_env, true, do_not_lookup );
            //
            // v. template_argument()

            std::cout << "template instantiation" << std::endl;

            return {
                id_detail.type_id,
                id_detail.target_env,
                nullptr,
                nullptr/*TODO: implement template*/
            };
        }



    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_IDENTIFIER_SOLVER_HPP*/
