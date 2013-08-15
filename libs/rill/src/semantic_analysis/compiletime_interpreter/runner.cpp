//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//


//
// Compile time interpreter
// runner
//


#include <rill/semantic_analysis/compiletime_interpreter/runner.hpp>
#include <rill/semantic_analysis/compiletime_interpreter/invoke.hpp>

#include <rill/semantic_analysis/invoke.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>

#include <rill/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            runner::runner( context_ptr const& ctx )
                : context_( ctx )
            {}

            // Root Scope
            void runner::operator()( ast::root const& ss, environment_ptr const& env ) const
            {
                collect_type_identifier( env, ss.statements_ );
                collect_identifier( env, ss.statements_ );

                for( auto const& node : ss.statements_ )
                    node->dispatch( *this,  env );
            }

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            void runner::operator()( ast::expression_statement const& s, environment_ptr const& env ) const
            {
                std::cout
                    << "in expression_statement dispach of runner" << std::endl
                    << s.expression_->dispatch( *this, env ) << std::endl;
                std::cout << "Value(current stack top) => " << *context_->current_stack_value() << std::endl;
            }

            void runner::operator()( ast::return_statement const& s, environment_ptr const& env ) const
            {
                s.expression_->dispatch( *this, env );

                //std::cout << "!!!!!!!" << s.expression_->dispatch( *this, env ) << std::endl;
                //context_->current_scope()->set_return_value( s.expression_->dispatch( *this, env ) );
            }

            void runner::operator()( ast::function_definition_statement const& s, environment_ptr const& env ) const
            {
                /*
                // TODO: remove this case in syntax analysis phase
                if ( s.get_identifier()->nest_size() != 1 )
                    std::cout << "function_definition_statement error!!!!!!! can not specified nested definition here." << std::endl;//error()


                // TODO: add steady step to check
                //     : OR CHANGE THE PARSER
                assert( s.get_identifier()->nest_size() == 1 ); // can not use nested type here


                // construct function environment in progress phase
                auto const& f_g_env
                        = env->construct(
                                kind::function_k,
                                s.get_identifier()->get_last_identifier(),
                                s.get_parameter_list(),
                                s.statements_
                                );
                assert( f_g_env != nullptr );
                auto const& f_env = std::dynamic_pointer_cast<function_symbol_environment>( f_g_env );

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
                }[]/
            }

            //
            analyse( f_env, s.statements_ );
            */

//            std::cout << "Function env is" << f_env << std::endl;
            // 
            //f_env->pre_construct( kind::variable_k, 
            }

            //void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

            void runner::operator()( ast::class_definition_statement const& s, environment_ptr const& env ) const
            {}


            // expression
            auto runner::operator()( ast::binary_operator_expression const& e, environment_ptr const& env ) const -> environment_ptr
            {
                // evaluate values(and push to stack) and returned value type
                auto const& lhs_type_env = e.lhs_->dispatch( *this, env );
                auto const& rhs_type_env = e.rhs_->dispatch( *this, env );

                //            std::cout
                //                << "in binary_operator_expression dispach of interpret_pass<runtime_interpret_tag>" << std::endl
                //                << e.op_->get_base_symbol()->get_native_string() << std::endl
                //                << "lhs" << lhs_type_env << std::endl
                //                << "rhs" << rhs_type_env << std::endl;
                //
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
                auto const& generic_function_env
                    = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment> const>( parameter_wrapper_env );
                //            std::cout << "function id: " << generif_function_env->get_id() << std::endl;

                // make argument types id list
                environment_id_list arg_type_env_ids;
                arg_type_env_ids.push_back( lhs_type_env->get_id() );
                arg_type_env_ids.push_back( rhs_type_env->get_id() );


                //
                auto const& f = generic_function_env->solve_overload( arg_type_env_ids );
                assert( f != nullptr );

                {
                    // make new scope
                    auto const& function_execution_scope = context_->push_new_scope();

                    // load values to callee function parameter variable
                    for( auto const& env_id : f->get_arg_load_env_ids() /* todo add reverse*/ )
                        context_->construct_variable( env_id, context_->pop_value().value );

                    // execute!!
                    for( auto const& node : f->get_statement_list() )
                        node->dispatch( *this, f );
                    //compiletime_run_on_context( context_, f, f->get_statement_list() );

                    // finalize
                    context_->pop_scope();
                }
                // construct scope and execute function
                //auto const& forward_scope = context_->push_entry_scope( args );
                //run_with_scope_pop( context_, f, f->get_statement_list(), is_on_compile_time_ );

                //            std::cout << "!!!!!!!" << forward_scope->get_return_value() << std::endl;

                // TODO: check status of scope(Ex. Exceptions...)

                /* auto const& ee = nullptr;// env->lookup_env( s.op_ );
                if ( !ee ) {
                // throw
                exit( -2 );
                }*/
                //auto const& return_value = forward_scope->get_return_value();

                return env->lookup( intrinsic::make_single_identifier( "int" ) ); // TODO: change to use function return type
                // f->return_type_identifier
            }

            //
            auto runner::operator()( ast::call_expression const& e, environment_ptr const& env ) const -> environment_ptr
            {
                // make function entry step


                // push values to context stack and evaluate type environment
                std::vector<environment_ptr> argument_type_env;
                for( auto const& val : e.arguments_ ) {
                    argument_type_env.push_back( val->dispatch( *this, env ) );
                }


                // TODO: support full identifier support
                auto const& parameter_wrapper_env = env->lookup( e.reciever_->get_last_identifier() );

                assert( parameter_wrapper_env != nullptr );
                assert( parameter_wrapper_env->get_symbol_kind() == kind::type_value::parameter_wrapper_e );
                assert( std::dynamic_pointer_cast<has_parameter_environment_base const>( parameter_wrapper_env )->get_inner_symbol_kind() == kind::type_value::function_e );


                //
                auto const& generic_function_env
                    = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment> const>( parameter_wrapper_env );
                //            std::cout << "function id: " << generif_function_env->get_id() << std::endl;


                // make argument types id list
                environment_id_list arg_type_env_ids;
                for( auto const& v : argument_type_env )
                    arg_type_env_ids.push_back( v->get_id() );

                // look up matched function
                auto const& f = generic_function_env->solve_overload( arg_type_env_ids );
                assert( f != nullptr );

                //            std::cout << "$%$%$%$%" << (environment_ptr)f << std::endl;

                {
                    // make new scope
                    auto const& function_execution_scope = context_->push_new_scope();

                    // load values to callee function parameter variable
                    for( auto const& env_id : f->get_arg_load_env_ids() /* todo add reverse*/ ) {
                        //                    std::cout << "=>> " << env_id << std::endl;
                        context_->construct_variable( env_id, context_->pop_value().value );
                    }

                    // execute!!
                    for( auto const& node : f->get_statement_list() )
                        node->dispatch( *this, f );
                    //compiletime_run_on_context( context_, f, f->get_statement_list() );

                    // finalize
                    context_->pop_scope();
                }


                //            std::cout << "&&&&&& call finished" << std::endl;

                //            std::cout << "!!!!!!!" << forward_scope->get_return_value() << std::endl;

                // TODO: check status of scope(Ex. Exceptions...)

                /* auto const& ee = nullptr;// env->lookup_env( s.op_ );
                if ( !ee ) {
                // throw
                exit( -2 );
                }*/

                auto return_value = context_->current_stack_value();

                return env->lookup( intrinsic::make_single_identifier( "int" ) ); // TODO: change to use function return type
            }

            //
            // embeded function must return values in intrinsic namespace.
            //
            auto runner::operator()( ast::embedded_function_call_expression const& e, environment_ptr const& env ) const -> environment_ptr
            {
                std::vector<const_value_ptr> args;
                args.push_back( context_->pop_value().value );
                args.push_back( context_->pop_value().value );

                //            std::cout << "stack debug" << std::endl;
                //            for( auto const& v : args )
                //                std::cout << "======" << std::endl << *v << std::endl;

                auto const& val = e.reciever_( args );
                assert( val != nullptr );

                auto const& typed_val = std::make_shared<intrinsic_value>( val );

                context_->push_value( val );

                //            ///
                //            std::cout
                //                << "in embedded_function_call_expression dispach of interpret_pass<runtime_interpret_tag>" << std::endl
                //                << *typed_val << std::endl;
                ///


                return env->lookup( typed_val->literal_type_name_ );
            }

            auto runner::operator()( ast::term_expression const& e, environment_ptr const& env ) const -> environment_ptr
            {
                return e.value_->dispatch( *this, env );
            }


            //
            auto runner::operator()( ast::intrinsic_value const& v, environment_ptr const& env ) const -> environment_ptr
            {
                //            std::cout << "Value => " << v << std::endl;
                context_->push_value( v.value_ );

                return env->lookup( v.literal_type_name_ );
            }

            auto runner::operator()( ast::variable_value const& v, environment_ptr const& env ) const -> environment_ptr
            {
                // lookup variable name environment
                auto const& val_env = env->nest_lookup( v.variable_name_ );
                assert( val_env != nullptr );
                assert( val_env->get_symbol_kind() == kind::type_value::variable_e );

                //            std::cout << "Variable ID => " << val_env->get_id() << std::endl;

                // get value by variable environment id
                auto const& ref_val = context_->get_variable_value_by_id( val_env->get_id() );
                assert( ref_val != nullptr );

                context_->push_value( ref_val );

                // return type environment
                return std::dynamic_pointer_cast<variable_symbol_environment>( val_env )->get_weak_type_env().lock();
            }

        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill