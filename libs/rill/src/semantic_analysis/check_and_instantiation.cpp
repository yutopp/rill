//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/check_and_instantiation_visitor.hpp>
#include <rill/semantic_analysis/invoke.hpp>

#include <rill/environment.hpp>

#include <rill/statement.hpp>
#include <rill/expression.hpp>
#include <rill/value.hpp>

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

            // construct function body
            auto const& f_env
                        = env->construct(
                            kind::function_k,
                            s.get_identifier()->get_last_identifier(),
                            s.get_parameter_list(),
                            s.statements_
                            );

            // 
            f_env->pre_construct( kind::variable_k, 
        }
        void check_and_instantiation_visitor::operator()( class_definition_statement const& s, environment_ptr const& env ) const
        {
        }
        
        // expression
        value_env_pair_t check_and_instantiation_visitor::operator()( binary_operator_expression const& e, environment_ptr const& env ) const
        {
            return nullexpr;
        }

        value_env_pair_t check_and_instantiation_visitor::operator()( call_expression const& e, environment_ptr const& env ) const
        {
            // check and instantiate nested identifier
            // TODO: instance nested

            const_environment_ptr const target_env
                = env->nest_lookup(
                    e.reciever_,
                    []( environment_ptr const& current_env, literal::single_identifier_value_base_ptr const& id ) {
                        if ( id->is_template() ) {
                            // TODO: add instatntiation
                            return nullptr;
                        } else {
                            std::cout << "noname ERROR!!!" << std::endl;
                            return nullptr;
                        }
                    } );

            environment_id_list ids;
            for( auto const& arg : e.arguments_ ) {
                auto const& val_env = arg->dispatch( *this, env );
                ids.push_back( val_env.env->get_id() );
            }

            auto const& pw = env->lookup( e.reciever_->get_last_identifier() );

            // TODO: check if we is template, instantiation
            if (false) {
                ;
            }

            if ( pw->get_symbol_kind() != kind::type_value::parameter_wrapper_e ) {
                std::cout << "noname ERROR!!!" << std::endl;
                return nullexpr;
            }
            if ( std::dynamic_pointer_cast<has_parameter_environment_base>( pw )->get_inner_symbol_kind() != kind::type_value::function_e ) {
                std::cout << "noname ERROR!!!" << std::endl;
                return nullexpr;
            }

            auto const& f = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment>>( pw );
            auto const& fb = f->solve_overload( ids );
            if ( fb == nullptr ) {
                std::cout << "noname ERROR!!!" << std::endl;
                return nullexpr;
            }

            // TODO: add implicit type comversin

            return make_value_env_pair( fb );
        }

        value_env_pair_t check_and_instantiation_visitor::operator()( embedded_function_call_expression const& e, environment_ptr const& env ) const
        {
            return nullexpr;
        }

        value_env_pair_t check_and_instantiation_visitor::operator()( term_expression const& e, environment_ptr const& env ) const
        {
            return nullexpr;
        }
        
        //
        const_environment_ptr check_and_instantiation_visitor::operator()( value const& v, environment_ptr const& env ) const
        {
            return nullptr;
        }

    } // namespace semantic_analysis
} // namespace rill