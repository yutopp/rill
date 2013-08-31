
#include <rill/semantic_analysis/semantic_analysis.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>

#include <rill/environment.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // Root Scope
        RILL_TV_OP( analyzer, ast::root, r, env )
        {
            // collect all type identifiers under this scope
            //collect_type_identifier( env, r.statements_ );

            // collect all identifiers(except types) under this scope
            collect_identifier( env, r.statements_ );

            std::cout << "ababab" << std::endl;

            // build environment
            for( auto const& node : r.statements_ )
                node->dispatch_as_env( *this,  env );
        }

        // statement
        // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

        RILL_TV_OP( analyzer, ast::expression_statement, s, env )
        {
            // DO NOT EVALUATE THIS PATH.
        }

        RILL_TV_OP( analyzer, ast::return_statement, s, env )
        {
            auto const r = s.expression_->dispatch_as_env( *this, env );

     
            std::cout << "!!!!!!!" << r << std::endl;
            //context_->current_scope()->set_return_value( s.expression_->dispatch( *this, env ) );
        }


        RILL_TV_OP( analyzer, ast::function_definition_statement, s, env )
        {
            // TODO: remove this case in syntax analysis phase
            if ( s.get_identifier()->nest_size() != 1 )
                std::cout << "function_definition_statement error!!!!!!! can not specified nested definition here." << std::endl;//error()

            // TODO: add steady step to check
            //     : OR CHANGE THE PARSER
            assert( s.get_identifier()->nest_size() == 1 ); // can not use nested type here


            // construct function environment in progress phase
            auto const& f_env = env->construct(
                kind::function_k,
                s.get_identifier()->get_last_identifier(),
                [&]( function_symbol_environment_ptr const& fenv ) {
                    // parameter variable declaration
                    for( auto const& e : s.get_parameter_list() ) {
                        // 
                        assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                        if ( e.decl_unit.init_unit.type ) { // is type specified ?
                            // evaluate constant expresison as type
                            auto const& type_identifier_pointer = interpreter::evaluate_as_type( env, e.decl_unit.init_unit.type );

                            if ( auto const type_env = lookup_with_instanciation( env, type_identifier_pointer ) ) {
                                assert( type_env != nullptr );
                                assert( type_env->get_symbol_kind() == kind::type_value::class_e );

                                // declare
                                fenv->parameter_variable_construct(
                                    /*TODO: add attributes, */
                                    nullptr,    // unnamed
                                    std::dynamic_pointer_cast<class_symbol_environment const>( type_env )
                                    );

                            } else {
                                // type was not found, compilation error
                                assert( false );
                            }

                        } else {
                            // type inferenced by result of evaluated expression

                            // TODO: implement type inference
                            assert( false );
                        }
                    }
                    return fenv;
            }, s.statements_ );

            assert( f_env != nullptr );

            // scan all statements in this function body
            // ?: TODO: use block expression

            // TODO: implement return type inference
            // TEMP: currently :int
            f_env->complete( env->lookup( intrinsic::make_single_identifier( "int" ) ) );
        }

        //void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

        RILL_TV_OP( analyzer, ast::class_definition_statement, s, env )
        {
        }



        //
        // expressions
        //


        //
        RILL_TV_OP( analyzer, ast::binary_operator_expression, e, env )
        {
            // check type environment
            auto const& lhs_type_env = e.lhs_->dispatch_as_env( *this, env );
            auto const& rhs_type_env = e.rhs_->dispatch_as_env( *this, env );

            assert( lhs_type_env != nullptr && rhs_type_env != nullptr );

            // find a function environment that has same name.
            auto const& target_env = env->lookup( e.op_ );

            // compilation errors
            if ( target_env == nullptr ) {
                // symbol not found;
                assert( false );
            }
            if ( target_env->get_symbol_kind() != kind::type_value::parameter_wrapper_e ) {
                // symbol type was not matched
                assert( false );
            }

            auto const has_parameter_env = std::dynamic_pointer_cast<has_parameter_environment_base const>( target_env );
            if ( has_parameter_env->get_inner_symbol_kind() != kind::type_value::function_e ) {
                // symbol type was not matched
                assert( false );
            }

            //
            auto const& generic_function_env
                = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment> const>( has_parameter_env );

            // make argument types id list
            environment_id_list arg_type_env_ids;
            arg_type_env_ids.push_back( lhs_type_env->get_id() );
            arg_type_env_ids.push_back( rhs_type_env->get_id() );

            // TODO: fix over load solver
            //
            auto const& function_env = generic_function_env->solve_overload( arg_type_env_ids );
            if ( function_env == nullptr ) {
                // overload failed
                assert( false );
            }

            // return retult type env of function
            return function_env->get_return_type_environment();
        }

        // function call expression
        RILL_TV_OP( analyzer, ast::call_expression, e, env )
        {
            // push values to context stack and evaluate type environment
            std::vector<environment_ptr> argument_type_env;
            for( auto const& val : e.arguments_ )
                argument_type_env.push_back( val->dispatch_as_env( *this, env ) );
            assert( std::count( argument_type_env.cbegin(), argument_type_env.cend(), nullptr ) == 0 );

            // find a function environment that has same name.
            auto const& target_env = lookup_with_instanciation( env, e.reciever_ );

            // compilation errors
            if ( target_env == nullptr ) {
                // symbol not found;
                assert( false );
            }
            if ( target_env->get_symbol_kind() != kind::type_value::parameter_wrapper_e ) {
                // symbol type was not matched
                assert( false );
            }

            auto const has_parameter_env = std::dynamic_pointer_cast<has_parameter_environment_base const>( target_env );
            if ( has_parameter_env->get_inner_symbol_kind() != kind::type_value::function_e ) {
                // symbol type was not matched
                assert( false );
            }

            //
            auto const& generic_function_env
                = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment> const>( has_parameter_env );

            // make argument types id list
            environment_id_list arg_type_env_ids;
            for( auto const& v : argument_type_env )
                arg_type_env_ids.push_back( v->get_id() );

            // TODO: fix over load solver
            //
            auto const& function_env = generic_function_env->solve_overload( arg_type_env_ids );
            if ( function_env == nullptr ) {
                // overload failed
                assert( false );
            }

            // return retult type env of function
            return function_env->get_return_type_environment();
        }

        //
        // embeded function must return values in intrinsic namespace.
        //
        RILL_TV_OP( analyzer, ast::embedded_function_call_expression, e, env )
        {
            // meybe unreachable
            assert( false );
            return nullptr;
        }

        RILL_TV_OP( analyzer, ast::term_expression, e, env )
        {
            return e.value_->dispatch_as_env( *this, env );
        }


        //
        RILL_TV_OP( analyzer, ast::intrinsic_value, v, env )
        {
            // look up literal type
            auto const type_env = env->lookup( v.literal_type_name_ );
            assert( type_env != nullptr );  // literal type must exist

            return type_env;
        }

        RILL_TV_OP( analyzer, ast::variable_value, v, env )
        {
            // unimplemented
            assert( false );
            return nullptr;
        }

    } // namespace semantic_analysis
} // namespace rill