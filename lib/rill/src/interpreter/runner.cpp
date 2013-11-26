//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <boost/range/adaptor/reversed.hpp>

#include <rill/interpreter/interpreter.hpp>
#include <rill/environment/environment.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>



namespace rill
{
    namespace interpreter
    {

        //
        runner::runner( context_ptr const& ctx, embedded_function_holder_ptr const& action_holder )
            : context_( ctx )
            , action_holder_( action_holder )
        {}


        // 
        RILL_TV_OP( runner, ast::root, r, env )
        {
            // TODO: call function from main routine
            // dispatch_as_env( r, *this, env );
            
            for( auto const& s : r->statements_ )
                dispatch( s, env );
        }


        // statement
        // virtual void operator()( template_statement const& s, environment_base_ptr const& env ) const =0;


        // 
        RILL_TV_OP( runner, ast::expression_statement, s, env )
        {
            dispatch( s->expression_, env );

            // erase stack
            context_->pop_value();

            std::cout << "<<=======" << std::endl;
        }


        //
        RILL_TV_OP( runner, ast::return_statement, s, env )
        {
            auto const& returned_env = dispatch( s->expression_, env );
            context_->current_scope()->set_return_state( returned_env->get_id(), context_->current_stack_value() );

            // std::cout << "return statement" << returned_env << std::endl;
        }


        //
        RILL_TV_OP( runner, ast::function_definition_statement, s, env )
        {
            // NOTHING TO DO
        }

        //void operator()( native_function_definition_statement const& s, environment_base_ptr const& env ) const =0;


        //
        RILL_TV_OP( runner, ast::class_definition_statement, s, env )
        {
            // NOTHING TO DO
        }


        // expression
        RILL_TV_OP( runner, ast::binary_operator_expression, e, env )
        {
            // evaluate values(and push to stack) and returned value type
            auto const& rhs_type_env = dispatch( e->rhs_, env );
            auto const& lhs_type_env = dispatch( e->lhs_, env );

//            std::cout
//                << "in binary_operator_expression dispach of interpret_pass<runtime_interpret_tag>" << std::endl
//                << e.op_->get_inner_symbol()->to_native_string() << std::endl
//                << "lhs" << lhs_type_env << std::endl
//                << "rhs" << rhs_type_env << std::endl;
//
//            std::cout << env << std::endl;


            //
            auto const& parameter_wrapper_env = env->lookup( e->op_ );
//            std::cout
//                << "search: " << e.op_->get_inner_symbol()->to_native_string() << std::endl
//                << parameter_wrapper_env->get_id() << std::endl
//                << static_cast<int>( parameter_wrapper_env->get_symbol_kind() ) << std::endl
//                ;

            //assert( parameter_wrapper_env != nullptr );
            //assert( parameter_wrapper_env->get_symbol_kind() == kind::type_value::parameter_wrapper_e );
            //assert( std::dynamic_pointer_cast<has_parameter_environment_base const>( parameter_wrapper_env )->get_inner_symbol_kind() == kind::type_value::function_e );

            //
            auto const& has_parameter_function_env
                = std::static_pointer_cast<has_parameter_environment<function_symbol_environment> const>( parameter_wrapper_env );
//            std::cout << "function id: " << generif_function_env->get_id() << std::endl;

            // make argument types( first argument to last ) id list
            environment_id_list arg_type_env_ids;
            arg_type_env_ids.push_back( lhs_type_env->get_id() );
            arg_type_env_ids.push_back( rhs_type_env->get_id() );

            //
            auto const& f_env = has_parameter_function_env->solve_overload( arg_type_env_ids );
            assert( f_env != nullptr );
            auto const& f_ast = std::static_pointer_cast<ast::function_definition_statement>( f_env->get_related_ast() );

            {
                auto const& prev_scope = context_->current_scope();

                // make new scope
                auto const& function_execution_scope = context_->push_new_scope();

                

                // load values to callee function parameter variable
                for( auto const& var_env_id : f_env->get_parameter_decl_ids() )
                    context_->construct_variable( variable_option::parameter_k, var_env_id, prev_scope->pop_value() );

                //std::cout << "f_env: " << (environment_base_ptr const&)f_env << std::endl;

                for( auto const& node : f_ast->statements_ ) {
                    dispatch( node, f_env );
                }

                auto const status = context_->current_scope()->get_return_status();
                // execute!!
                // run_on_context( context_, f, f->get_statement_list(), is_on_compile_time_ );

                // TODO: add value conversion
                prev_scope->push_value( status->second );

                auto const& val_env_id = status->first;
                auto const& variable_decl_env = std::static_pointer_cast<variable_symbol_environment>( env->get_env_at( val_env_id ).lock() );
                // TODO: variable_decl_env->get_type() and check return type of this funciton

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

            return f_env->get_return_type_environment();
        }



        RILL_TV_OP( runner, ast::call_expression, e, env )
        {
            std::vector<environment_base_ptr> argument_type_env;
            for( auto const& val : e->arguments_ | boost::adaptors::reversed ) {
                argument_type_env.insert( argument_type_env.begin(), dispatch( val, env ) );
            }

            

            // TODO: support full identifier support
            auto const& parameter_wrapper_env = env->lookup( e->reciever_->get_last_identifier() );

//            assert( parameter_wrapper_env != nullptr );
//            assert( parameter_wrapper_env->get_symbol_kind() == kind::type_value::parameter_wrapper_e );
//            assert( std::dynamic_pointer_cast<has_parameter_environment_base const>( parameter_wrapper_env )->get_inner_symbol_kind() == kind::type_value::function_e );


            //
            auto const& has_parameter_function_env
                = std::static_pointer_cast<has_parameter_environment<function_symbol_environment> const>( parameter_wrapper_env );
//            std::cout << "function id: " << generif_function_env->get_id() << std::endl;
            

            // make argument types id list
            environment_id_list arg_type_env_ids;
            for( auto const& v : argument_type_env )
                arg_type_env_ids.push_back( v->get_id() );

            //
            auto const& f_env = has_parameter_function_env->solve_overload( arg_type_env_ids );
            assert( f_env != nullptr );
            auto const& f_ast = std::static_pointer_cast<ast::function_definition_statement>( f_env->get_related_ast() );

            

            {
                auto const& prev_scope = context_->current_scope();

                // make new scope
                auto const& function_execution_scope = context_->push_new_scope();

                std::cout << "bunbun: " << (environment_base_ptr const&)f_env << std::endl << f_env->get_parameter_decl_ids().size() << std::endl;

                // load values to callee function parameter variable
                for( auto const& var_env_id : f_env->get_parameter_decl_ids() )
                    context_->construct_variable( variable_option::parameter_k, var_env_id, prev_scope->pop_value() );

                //std::cout << "f_env: " << (environment_base_ptr const&)f_env << std::endl;

                for( auto const& node : f_ast->statements_ ) {
                    dispatch( node, f_env );
                }

                auto const status = context_->current_scope()->get_return_status();
                // execute!!
                // run_on_context( context_, f, f->get_statement_list(), is_on_compile_time_ );

                // TODO: add value conversion
                prev_scope->push_value( status->second );

                auto const& val_env_id = status->first;
                auto const& variable_decl_env = std::static_pointer_cast<variable_symbol_environment>( env->get_env_at( val_env_id ).lock() );
                // TODO: variable_decl_env->get_type() and check return type of this funciton

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

            return f_env->get_return_type_environment();
        }


        //
        // embeded function must return values in intrinsic namespace.
        //
        RILL_TV_OP( runner, ast::embedded_function_call_expression, e, env )
        {
//            std::cout << "stack debug" << std::endl;
//            for( auto const& v : args )
//                std::cout << "======" << std::endl << *v << std::endl;

            auto const& action = action_holder_->at( e->action_id_ );

            auto const& val = action->invoke( processing_context::debug_interpreter_k, context_ );
            assert( val != nullptr );

            auto const& typed_val = std::make_shared<ast::intrinsic_value>( val );

            context_->push_value( val );

//            ///
//            std::cout
//                << "in embedded_function_call_expression dispach of interpret_pass<runtime_interpret_tag>" << std::endl
//                << *typed_val << std::endl;
            ///


            return env->lookup( typed_val->literal_type_name_ );
        }

        //
        RILL_TV_OP( runner, ast::term_expression, e, env )
        {
            return dispatch( e->value_, env );
        }


        RILL_TV_OP( runner, ast::type_identifier_expression, e, env )
        {
            assert( false );
            return nullptr;
        }

        RILL_TV_OP( runner, ast::compiletime_return_type_expression, e, env )
        {
            assert( false );
            return nullptr;
        }


        //
        RILL_TV_OP( runner, ast::intrinsic_value, v, env )
        {
            context_->push_value( v->value_ );

            return env->lookup( v->literal_type_name_ );
        }


        //
        RILL_TV_OP( runner, ast::variable_value, v, env )
        {
            // lookup variable name environment
            auto const& val_env = std::static_pointer_cast<variable_symbol_environment>( env->nest_lookup( v->variable_name_ ) );
            // assert( val_env != nullptr );
            // assert( val_env->get_symbol_kind() == kind::type_value::variable_e );

//            std::cout << "Variable ID => " << val_env->get_id() << std::endl;

            // get value by variable environment id
            auto const& ref_val = context_->get_variable_value_by_id( val_env->get_id() );
            assert( ref_val != nullptr );

            context_->push_value( ref_val );

            // return type environment
            return env->get_env_at( val_env->get_type_env_id() ).lock();
        }

    } // namespace interpreter
} // namespace rill
