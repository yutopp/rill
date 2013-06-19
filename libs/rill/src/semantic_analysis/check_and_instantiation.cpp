//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/check_and_instantiation_visitor.hpp>
#include <rill/semantic_analysis/invoke.hpp>
#include <rill/semantic_analysis/helper.hpp>

#include <rill/environment.hpp>

#include <rill/statement.hpp>
#include <rill/expression.hpp>
#include <rill/value.hpp>

#include <boost/range/adaptor/transformed.hpp>

namespace rill
{
    namespace semantic_analysis
    {
        // statement_list
        void check_and_instantiation_visitor::operator()( statement_list const& ss, environment_ptr const& env ) const
        {
            for( auto const& s : ss )
                s->dispatch( *this, env );
        }
        
        void check_and_instantiation_visitor::operator()( expression_statement const& s, environment_ptr const& env ) const
        {
        }
        void check_and_instantiation_visitor::operator()( return_statement const& s, environment_ptr const& env ) const
        {
        }
        void check_and_instantiation_visitor::operator()( function_definition_statement const& s, environment_ptr const& env ) const
        {
            if ( s.get_identifier()->nest_size() != 1 )
                std::cout << "function_definition_statement error!!!!!!!" << std::endl;//error()

            // instantiation  !!! TYPES !!! parameter
            bool is_type_instantiation_succeeded = true;
            for( auto const& e : s.get_parameter_list() ) {
                assert( e.type != nullptr );
                if ( !lookup_with_instanciation( env, e.type ) )
                    is_type_instantiation_succeeded = false;
            }
            if ( !is_type_instantiation_succeeded ) {
                std::cout << "type parameter is not found....." << std::endl;
                exit( -999 );
            }



            // TODO: add steady step to check
            //     : OR CHANGE THE PARSER
            assert( s.get_identifier()->nest_size() == 1 ); // can not use nested type here

            // construct function body
            auto const& f_g_env
                        = env->construct(
                            kind::function_k,
                            s.get_identifier()->get_last_identifier(),
                            s.get_parameter_list(),
                            s.statements_
                            );
            assert( f_g_env != nullptr );
            auto const& f_env = std::dynamic_pointer_cast<function_symbol_environment>( f_g_env );

            // construct argument variable symbol
            for( auto const& e : s.get_parameter_list() ) {
                assert( e.name != nullptr );
                // TODO: add steady step to check
                //     : OR CHANGE THE PARSER
                assert( e.name->nest_size() == 1 ); // can not use nested type here

                // construct parameter ariable
                auto const val_env = f_env->construct( kind::variable_k, e.name->get_last_identifier(), env->nest_lookup( e.type )->get_id() );
                assert( f_env != 0 );

                // specify entry variable holder
                f_env->push_arg_load_env_id( val_env->get_id() );

                // TODO: add step to despatch default values...
/*                auto const def_value = e.default_value;
                if ( def_value ) {
                } else {
                }*/
            }

            //
            analyse( f_env, s.statements_ );


            std::cout << env << std::endl;
            // 
            //f_env->pre_construct( kind::variable_k, 
        }
        void check_and_instantiation_visitor::operator()( class_definition_statement const& s, environment_ptr const& env ) const
        {
        }
        
        // expression
        auto check_and_instantiation_visitor::operator()( binary_operator_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }

        auto check_and_instantiation_visitor::operator()( call_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            // check and instantiate nested identifier
            // TODO: instance nested

            const_environment_ptr const target_env = lookup_with_instanciation( env, e.reciever_ );

            environment_id_list ids;
            for( auto const& arg : e.arguments_ ) {
                auto const& val_env = arg->dispatch( *this, env );
                ids.push_back( val_env->get_id() );
            }

            auto const& pw = env->lookup( e.reciever_->get_last_identifier() );

            // TODO: check if we is template, instantiation
            if (false) {
                ;
            }

            if ( pw->get_symbol_kind() != kind::type_value::parameter_wrapper_e ) {
                std::cout << "noname ERROR!!!" << std::endl;
                return nullptr;
            }
            if ( std::dynamic_pointer_cast<has_parameter_environment_base>( pw )->get_inner_symbol_kind() != kind::type_value::function_e ) {
                std::cout << "noname ERROR!!!" << std::endl;
                return nullptr;
            }

            auto const& f = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment>>( pw );

            //
            auto const& fb = f->solve_overload( ids );
            if ( fb == nullptr ) {
                std::cout << "noname ERROR!!!" << std::endl;
                return nullptr;
            }

            // TODO: add implicit type comversin

            return fb;
        }

        auto check_and_instantiation_visitor::operator()( embedded_function_call_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }

        auto check_and_instantiation_visitor::operator()( term_expression const& e, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }
        
        //
        auto check_and_instantiation_visitor::operator()( intrinsic_value const& v, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }
        auto check_and_instantiation_visitor::operator()( variable_value const& s, environment_ptr const& env ) const -> environment_ptr
        {
            return nullptr;
        }
    } // namespace semantic_analysis
} // namespace rill