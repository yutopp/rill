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
        RILL_TV_OP( analyzer, ast::root, r, parent_env )
        {
            // collect all type identifiers under this scope
            //collect_type_identifier( env, r.statements_ );

            // collect all identifiers(except types) under this scope
            collect_identifier( parent_env, r );

            std::cout << "ababab" << std::endl << parent_env << std::endl;

            // build environment
            for( auto const& node : r->statements_ )
                dispatch( node, parent_env );
        }

        // statement
        // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

        RILL_TV_OP( analyzer, ast::expression_statement, s, parent_env )
        {
            // // DO NOT EVALUATE THIS PATH.
            dispatch( s->expression_, parent_env );
        }

        RILL_TV_OP( analyzer, ast::return_statement, s, parent_env )
        {
            auto const r = dispatch( s->expression_, parent_env );

            std::cout << "!!!!!!!" << r << std::endl;
            //context_->current_scope()->set_return_value( s.expression_->dispatch( *this, env ) );
        }

        RILL_TV_OP( analyzer, ast::variable_declaration_statement, s, parent_env )
        {
            //
            bool const is_backward_reference
                = parent_env->get_symbol_kind() == kind::type_value::function_e;

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            if ( is_backward_reference ) {
                //
                if ( related_env == nullptr ) {
//                    assert( false );

                    auto const& val_decl = s->declaration_;
                    // TODO: decl_unit will be unit_list
                    // for( auto const& unit : val_decl.decl_unit_list ) {
                    auto const& unit = val_decl.decl_unit;

                    // TODO: make method to determine "type"

                    // unit.kind -> val or ref
                    // TODO: use unit.kind( default val )
                    
                    // TODO: evaluate type || type inference || type check
                    //       default( int )

                    if ( unit.init_unit.type ) { // is parameter variavle type specified ?
                        // evaluate constant expresison as type
                        auto const& type_identifier_pointer = interpreter::evaluate_as_type( parent_env, unit.init_unit.type );

                        if ( auto const type_env = lookup_with_instanciation( parent_env, type_identifier_pointer ) ) {
                            assert( type_env != nullptr );
                            assert( type_env->get_symbol_kind() == kind::type_value::class_e );

                            // declare
                            auto variable_env
                                = parent_env->construct(
                                    kind::variable_k,
                                    /*TODO: add attributes, */
                                    unit.name,
                                    std::dynamic_pointer_cast<class_symbol_environment const>( type_env )
                                    );

                            //
                            variable_env->connect_from_ast( s );

                        } else {
                            // type was not found, !! compilation error !!
                            assert( false );
                        }

                    } else {
                        // type inferenced by result of evaluated [[default initializer expression]]

                        // TODO: implement type inference
                        assert( false );
                    }

                    

                } else {
                    // Already declared...(Duplicate)
                    assert( false );
                }


            } else {
                // TODO: implement
                assert( false );
            }
            

//            assert( related_env != nullptr );
//            assert( related_env->get_symbol_kind() == kind::type_value::function_e );


/*
            std::cout
                << "variable_declaration_statement: ast_ptr -> "
                << (environment_ptr const&)parent_env << std::endl
                << "name -- " << s->declaration_->get_identifier()->last()->get_inner_symbol()->to_native_string() << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;
*/
        }


        RILL_TV_OP( analyzer, ast::function_definition_statement, s, parent_env )
        {
            std::cout
                << "function_definition_statement: ast_ptr -> "
                << (environment_ptr const&)parent_env << std::endl
                << "name -- " << s->get_identifier()->last()->get_inner_symbol()->to_native_string() << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::function_e );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->check();

            // construct function environment in progress phase

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
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
                        // type was not found, !! compilation error !!
                        assert( false );
                    }

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    assert( false );
                }
            }

            // scan all statements in this function body
            for( auto const& node : s->statements_ )
                dispatch( node, f_env );
            // ?: TODO: use block expression


            auto const& return_type_env
                = s->return_type_
                ? lookup_with_instanciation( f_env, *s->return_type_ )
                : []() {
                    // TODO: implement return type inference
                    assert( false );
                    return nullptr;
                }();

            f_env->complete( return_type_env, s->get_identifier()->last()->get_inner_symbol()->to_native_string() );

            //
            f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_ptr const)f_env << std::endl;
        }

        RILL_TV_OP( analyzer, ast::extern_function_declaration_statement, s, parent_env )
        {
            std::cout
                << "function_definition_statement: ast_ptr -> "
                << (environment_ptr const&)parent_env << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

            // enverinment is already pre constructed by identifier_collector
            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::function_e );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->check();

            // construct function environment in progress phase

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
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
                        // type was not found, !! compilation error !!
                        assert( false );
                    }

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    assert( false );
                }
            }

            // TODO: implement return type inference
            auto const& return_type_env
                = s->return_type_
                ? lookup_with_instanciation( f_env, *s->return_type_ )
                : []() {
                    // NOTE: it should be compilation error on extern_function_declaration_statement
                    assert( false );
                    return nullptr;
                }();

            f_env->complete( return_type_env, s->get_identifier()->last()->get_inner_symbol()->to_native_string(), function_symbol_environment::attr::e_extern );

            //
            f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_ptr const)f_env << std::endl;
        }



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
            auto const& lhs_type_env = dispatch( e->lhs_, env );
            auto const& rhs_type_env = dispatch( e->rhs_, env );

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

            // memoize called function env
            function_env->connect_from_ast( e );

            // return retult type env of function
            return function_env->get_return_type_environment();
        }

        // function call expression
        RILL_TV_OP( analyzer, ast::call_expression, e, env )
        {
            // push values to context stack and evaluate type environment
            std::vector<environment_ptr> argument_type_env;
            for( auto const& val : e->arguments_ )
                argument_type_env.push_back( dispatch( val, env ) );
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
                function_env->connect_from_ast( e );

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
                    dispatch( statement_node, incomplete_function_env->get_parent_env() );
                }

                // retry
                auto const& function_env = has_parameter_function_env->solve_overload( arg_type_env_ids );
                if ( function_env ) {
                    function_env->connect_from_ast( e );

                    // returns return value type envitonment
                    return function_env->get_return_type_environment();

                } else {
                    // overload failed
                    assert( false );
                }
            }

            // memoize called function env
            std::cout << "memoed" << std::endl;
            function_env->connect_from_ast( e );
            
            // return retult type env of function
            //return function_env->get_return_type_environment();
        }

        RILL_TV_OP( analyzer, ast::term_expression, e, env )
        {
            return dispatch( e->value_, env );
        }


        //
        RILL_TV_OP( analyzer, ast::intrinsic_value, v, env )
        {
            // look up literal type
            auto const type_env = env->lookup( v->literal_type_name_ );
            assert( type_env != nullptr );  // literal type must exist

            return type_env;
        }

        RILL_TV_OP( analyzer, ast::variable_value, v, parent_env )
        {
            auto const& target_env = lookup_with_instanciation( parent_env, v->variable_name_ );
            if ( target_env == nullptr ) {
                // compilation error
                assert( false );
            }

            if ( target_env->get_symbol_kind() != kind::type_value::variable_e ) {
                // symbol type was not matched
                assert( false );
            }

            auto const& variable_env = std::static_pointer_cast<variable_symbol_environment>( target_env );

            // memoize
            std::cout << "memoed" << std::endl;
            variable_env->connect_from_ast( v );

            auto const& type_env = parent_env->get_env_at( variable_env->get_type_env_id() ).lock();
            assert( type_env != nullptr );
            std::cout << type_env->mangled_name() << std::endl;
            return type_env;
        }

    } // namespace semantic_analysis
} // namespace rill
