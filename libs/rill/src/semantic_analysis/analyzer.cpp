//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/environment.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>




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
            collect_identifier( env, r );

            std::cout << "ababab" << std::endl << env << std::endl;

            // build environment
            for( auto const& node : r->statements_ )
                dispatch_as_env( node, *this,  env );
        }

        // statement
        // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

        RILL_TV_OP( analyzer, ast::expression_statement, s, env )
        {
            // // DO NOT EVALUATE THIS PATH.
            dispatch_as_env( s->expression_, *this, env );
        }

        RILL_TV_OP( analyzer, ast::return_statement, s, env )
        {
            auto const r = dispatch_as_env( s->expression_, *this, env );

            std::cout << "!!!!!!!" << r << std::endl;
            //context_->current_scope()->set_return_value( s.expression_->dispatch( *this, env ) );
        }


        RILL_TV_OP( analyzer, ast::function_definition_statement, s, env )
        {
            std::cout
                << "function_definition_statement: ast_ptr -> "
                << (environment_ptr const&)env << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const r_env = env->get_related_env_by_ast_ptr( s );
            assert( r_env != nullptr );
            assert( r_env->get_symbol_kind() == kind::type_value::function_e );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( r_env );
            assert( f_env != nullptr );

            if ( !f_env->is_incomplete() )
                return;

            // construct function environment in progress phase
            for( auto const& e : s->get_parameter_list() ) {
                // 
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is type specified ?
                    // evaluate constant expresison as type
                    auto const& type_identifier_pointer = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    if ( auto const type_env = lookup_with_instanciation( f_env, type_identifier_pointer ) ) {
                        assert( type_env != nullptr );
                        assert( type_env->get_symbol_kind() == kind::type_value::class_e );

                        // declare
                        f_env->parameter_variable_construct(
                            /*TODO: add attributes, */
                            e.decl_unit.name,
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

            // scan all statements in this function body
            // ?: TODO: use block expression

            // TODO: implement return type inference
            // TEMP: currently :int
            f_env->complete( env->lookup( intrinsic::make_single_identifier( "int" ) ) );

            //
            f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_ptr const)f_env << std::endl;
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
            auto const& lhs_type_env = dispatch_as_env( e->lhs_, *this, env );
            auto const& rhs_type_env = dispatch_as_env( e->rhs_, *this, env );

            assert( lhs_type_env != nullptr && rhs_type_env != nullptr );

            // find a function environment that has same name.
            auto const& target_env = env->lookup( e->op_ );

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
            for( auto const& val : e->arguments_ )
                argument_type_env.push_back( dispatch_as_env( val, *this, env ) );
            assert( std::count( argument_type_env.cbegin(), argument_type_env.cend(), nullptr ) == 0 );

            // TODO: fix lookup phase
            // find a function environment that has same name.
            auto const& target_env = lookup_with_instanciation( env, e->reciever_ );

            // compilation errors
            if ( target_env == nullptr ) {
                // symbol not found
                // ?: look up 1 rank top environment or other namespace groups
                assert( false );
            }
            if ( target_env->get_symbol_kind() != kind::type_value::parameter_wrapper_e ) {
                // symbol type was not matched
                assert( false );
            }

            auto const has_parameter_env = std::static_pointer_cast<has_parameter_environment_base>( target_env );
            if ( has_parameter_env->get_inner_symbol_kind() != kind::type_value::function_e ) {
                // symbol type was not matched
                assert( false );
            }

            //
            auto const& has_parameter_function_env
                = std::static_pointer_cast<has_parameter_environment<function_symbol_environment>>( has_parameter_env );

            // make argument types id list
            environment_id_list arg_type_env_ids;
            for( auto const& v : argument_type_env )
                arg_type_env_ids.push_back( v->get_id() );


            // TODO: fix over load solver
            //
            auto const& function_env = has_parameter_function_env->solve_overload( arg_type_env_ids );
            if ( function_env ) {
                // returns return value type envitonment
                return function_env->get_return_type_environment();

            } else {
                // 
                //if ( has_parameter_function_env->get_parent_env()->is_root() )
                //    assert( false );

                /// solve_forward_reference( has_parameter_env, env, e );
                for( auto const& incomplete_function_env : has_parameter_function_env->get_incomplete_inners() ) {
                    assert( incomplete_function_env != nullptr );
                    std::cout << "found marked(ast AND related env) -> " << incomplete_function_env->get_id() << std::endl;

                    auto const& statement_node = incomplete_function_env->get_related_ast();
                    assert( statement_node != nullptr );

                    // to complate incomplete_funciton_env( after that, incomplete_function_env will be complete_function_env)
                    dispatch_as_env( statement_node, *this, incomplete_function_env->get_parent_env() );
                }

                // retry
                auto const& function_env = has_parameter_function_env->solve_overload( arg_type_env_ids );
                if ( function_env ) {
                    // returns return value type envitonment
                    return function_env->get_return_type_environment();

                } else {
                    // overload failed
                    assert( false );
                }
            }

            
            // return retult type env of function
            //return function_env->get_return_type_environment();
        }


        RILL_TV_OP( analyzer, ast::term_expression, e, env )
        {
            return dispatch_as_env( e->value_, *this, env );
        }


        //
        RILL_TV_OP( analyzer, ast::intrinsic_value, v, env )
        {
            // look up literal type
            auto const type_env = env->lookup( v->literal_type_name_ );
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
