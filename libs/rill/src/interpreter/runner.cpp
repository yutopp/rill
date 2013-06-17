//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/interpreter/runner.hpp>
#include <rill/interpreter/invoke.hpp>

#include <rill/environment.hpp>

#include <rill/statement.hpp>
#include <rill/expression.hpp>
#include <rill/value.hpp>

namespace rill
{
    namespace interpreter
    {
        runner::runner( context_ptr const& ctx, bool const is_on_compile_time )
            : context_( ctx )
            , is_on_compile_time_( is_on_compile_time )
        {}

        // statement_list
        void runner::operator()( statement_list const& ss, environment_ptr const& env ) const
        {
            // TODO: add return step
            for( auto const& s : ss )
                s->dispatch( *this, env );
        }

        // statement
        // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

        void runner::operator()( expression_statement const& s, environment_ptr const& env ) const
        {
            std::cout
                << "in expression_statement dispach of runner" << std::endl
                << s.expression_->dispatch( *this, env ) << std::endl;
        }

        void runner::operator()( return_statement const& s, environment_ptr const& env ) const
        {
//            std::cout << "!!!!!!!" << s.expression_->dispatch( *this, env ) << std::endl;
            context_->current_scope()->set_return_value( s.expression_->dispatch( *this, env ) );
        }

        void runner::operator()( function_definition_statement const& s, environment_ptr const& env ) const
        {}

        //void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

        void runner::operator()( class_definition_statement const& s, environment_ptr const& env ) const
        {}

        // expression
        value_env_pair_t runner::operator()( binary_operator_expression const& e, environment_ptr const& env ) const
        {
            auto const& evaled_lhs = e.lhs_->dispatch( *this, env );
            auto const& evaled_rhs = e.rhs_->dispatch( *this, env );

//            std::cout
//                << "in binary_operator_expression dispach of interpret_pass<runtime_interpret_tag>" << std::endl
//                << evaled_lhs << std::endl
//                << evaled_rhs << std::endl;

//            std::cout << env << std::endl;


            //
            auto const& parameter_wrapper_env = env->lookup( e.op_ );
//            std::cout
//                << "search: " << e.op_->get_base_symbol()->get_native_string() << std::endl
//                << parameter_wrapper_env->get_id() << std::endl
//                << static_cast<int>( parameter_wrapper_env->get_symbol_kind() ) << std::endl
//                ;

            assert( parameter_wrapper_env != nullptr );
            assert( parameter_wrapper_env->get_symbol_kind() == kind::type_value::parameter_wrapper_e );
            assert( std::dynamic_pointer_cast<has_parameter_environment_base const>( parameter_wrapper_env )->get_inner_symbol_kind() == kind::type_value::function_e );

            //
            auto const& generif_function_env = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment> const>( parameter_wrapper_env );
//            std::cout << "function id: " << generif_function_env->get_id() << std::endl;

            environment_id_list ids;
            ids.push_back( evaled_lhs.env->get_id() );
            ids.push_back( evaled_lhs.env->get_id() );

            auto const& f = generif_function_env->solve_overload( ids );
            assert( f != nullptr );

            argument_list args;
            args.push_back( evaled_lhs.value );
            args.push_back( evaled_rhs.value );

            // construct scope and execute function
            auto const& forward_scope = context_->push_entry_scope( args );
            run_with_scope_pop( context_, f, f->get_statement_list(), is_on_compile_time_ );

//            std::cout << "!!!!!!!" << forward_scope->get_return_value() << std::endl;

            // TODO: check status of scope(Ex. Exceptions...)
           
            /* auto const& ee = nullptr;// env->lookup_env( s.op_ );
            if ( !ee ) {
                // throw
                exit( -2 );
            }*/
            auto const& return_value = forward_scope->get_return_value();

            return return_value;
        }

        value_env_pair_t runner::operator()( call_expression const& e, environment_ptr const& env ) const
        {
            std::vector<value_env_pair_t> evaled_values;
            for( auto const& v : e.arguments_ )
                evaled_values.push_back( v->dispatch( *this, env ) );

            // TODO: support full identifier support
            auto const& parameter_wrapper_env = env->lookup( e.reciever_->get_last_identifier() );

            assert( parameter_wrapper_env != nullptr );
            assert( parameter_wrapper_env->get_symbol_kind() == kind::type_value::parameter_wrapper_e );
            assert( std::dynamic_pointer_cast<has_parameter_environment_base const>( parameter_wrapper_env )->get_inner_symbol_kind() == kind::type_value::function_e );

            auto const& generif_function_env
                = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment> const>( parameter_wrapper_env );
//            std::cout << "function id: " << generif_function_env->get_id() << std::endl;

            // 
            environment_id_list ids;
            for( auto const& v : evaled_values )
                ids.push_back( v.env->get_id() );

            auto const& f = generif_function_env->solve_overload( ids );
            assert( f != nullptr );

            argument_list args;
            for( auto const& v : evaled_values )
                args.push_back( v.value );

            // construct scope and execute function
            auto const& forward_scope = context_->push_entry_scope( args );

            //
            auto const& loader_env_ids = f->get_arg_load_env_ids();
            for( std::size_t i=0; i<loader_env_ids.size(); ++i ) {
                forward_scope->set_local_value( loader_env_ids[i], args[i] );
            }
            //f->
            run_with_scope_pop( context_, f, f->get_statement_list(), is_on_compile_time_ );

//            std::cout << "!!!!!!!" << forward_scope->get_return_value() << std::endl;

            // TODO: check status of scope(Ex. Exceptions...)
           
            /* auto const& ee = nullptr;// env->lookup_env( s.op_ );
            if ( !ee ) {
                // throw
                exit( -2 );
            }*/
            auto const& return_value = forward_scope->get_return_value();

            return return_value;
        }

        value_env_pair_t runner::operator()( embedded_function_call_expression const& e, environment_ptr const& env ) const
        {
            auto const& val = e.reciever_( context_->current_scope()->get_args() );
            value_env_pair_t ve = { val, val->dispatch( *this, env ) };
/*
            std::cout
                << "in embedded_function_call_expression dispach of interpret_pass<runtime_interpret_tag>" << std::endl
                << ve << std::endl;*/

            return ve;
        }

        value_env_pair_t runner::operator()( term_expression const& e, environment_ptr const& env ) const
        {
            value_env_pair_t ve = { e.value_, e.value_->dispatch( *this, env ) };

            return ve;
        }

        //
        const_environment_ptr runner::operator()( value const& v, environment_ptr const& env ) const
        {
            if ( v.is_intrinsic_type() ) {
                return env->lookup_env_on_root( v.intrinsic_typed_identifier_ );

            } else {
                // TODO: insert step to search symbol
                return nullptr;
            }
        }

    } // namespace interpreter
} // namespace rill