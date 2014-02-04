//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/analyzer_type.hpp>
#include <rill/environment/environment.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        //
        //
        //
        RILL_TV_OP( analyzer, ast::nested_identifier_value, v, parent_env )
        {
            assert( false && "[[ICE]] not supported");
        }


        //
        //
        //
        RILL_TV_OP( analyzer, ast::identifier_value, v, parent_env )
        {
            std::cout << "Find Identifier: " << v->get_inner_symbol()->to_native_string() << std::endl;
            auto const target_env = parent_env->lookup( v );
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
                variable_env->connect_from_ast( v );

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
                auto const& type_class_env = root_env_->lookup( ast::make_identifier( "type" ) );
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



        //
        //
        //
        RILL_TV_OP( analyzer, ast::template_instance_value, v, env )
        {

        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::literal_value, v, env )
        {
            // look up literal type
            // TODO: look up on ROOT
            auto const class_env = env->lookup( v->literal_type_name_ );
            assert( class_env != nullptr );  // literal type must exist

            //
            return {
                class_env->make_type_id( class_env, determine_type_attributes() ),
                class_env
            };
        }

    } // namespace semantic_analysis
} // namespace rill
