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

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>

#include <vector>
#include <boost/range/adaptor/transformed.hpp>


namespace rill
{
    namespace semantic_analysis
    {
struct to_type_id_t
{
    typedef type_id_t result_type;

template<typename T>
auto operator()(T const& c) const 
-> result_type
    {
return c.type_id;
}
};
        //
        RILL_TV_OP( analyzer, ast::binary_operator_expression, e, env )
        {
            using namespace boost::adaptors;
    
            // check type environment
            auto const& lhs_type_id_with_env
                = dispatch( e->lhs_, env );
            auto const& rhs_type_id_with_env
                = dispatch( e->rhs_, env );

            // make argument types id list
            std::vector<type_id_with_env> const& argument_type_ids_with_envs
                = { lhs_type_id_with_env, rhs_type_id_with_env };

            // TODO: check type_id_special
            assert(
                std::count_if(
                    argument_type_ids_with_envs.cbegin(),
                    argument_type_ids_with_envs.cend(), []( type_id_with_env const& t ) {
                        return t.type_id == type_id_undefined
                            || t.type_id == type_id_special;
                    }
                    ) == 0
                );

            // DEBUG
            std::cout << "...resolving function : " << e->op_->get_inner_symbol()->to_native_string() << std::endl;


            // TODO: 1. call lhs.operator( rhs );
            // 2. call operator( lhs, rhs );

            // find a function environment that has same name
            auto const& target_env = env->lookup( e->op_ );
            // compilation errors
            if ( target_env == nullptr ) {
                // symbol not found;
                assert( false && "symbol not found" );
            }
            if ( target_env->get_symbol_kind() != kind::type_value::e_parameter_wrapper ) {
                // symbol type was not matched
                assert( false && "[ice]");
            }

            auto has_parameter_env = std::dynamic_pointer_cast<has_parameter_environment_base>( target_env );
            if ( has_parameter_env->get_inner_symbol_kind() != kind::type_value::e_function ) {
                // symbol type was not matched
                assert( false && "[ice]" );
            }
            // this environment has all functions that have same identifier
            auto generic_function_env
                = std::static_pointer_cast<has_parameter_environment<function_symbol_environment>>( has_parameter_env );


            // TODO: make this section to loop
            //       generic_function_env has only one scope. should check parent environment.
            auto const& function_env = [&](){
                std::cout << (const_environment_base_ptr)env << std::endl;

                /// *************
                auto const& f = overload_solver(
                    argument_type_ids_with_envs | transformed( to_type_id_t() ),
                    generic_function_env,
                    env
                    );
                // TODO: null check and do loop

                return f;
            }();
/*
            std::cout << "...call expression : " << e->op_->get_inner_symbol()->to_native_string() << std::endl;
            assert( false && "Overload failed..." );


            auto const& function_env = generic_function_env->solve_overload( arg_type_ids );
            if ( function_env == nullptr ) {
                // overload failed
                std::cout << "...call expression : " << e->op_->get_inner_symbol()->to_native_string() << std::endl;
                assert( false && "Overload failed..." );
            }
*/

            // memoize called function env
            function_env->connect_from_ast( e );

            // return retult type env of function
            return {
                function_env->get_return_type_id(),
                function_env
            };
        }





        //
        //
        //
        RILL_TV_OP( analyzer, ast::element_selector_expression, e, parent_env )
        {
            // ========================================
            //
            auto const& reciever_type_id_with_env
                = dispatch( e->reciever_, parent_env );

            // ========================================
            // FIXME: support function...
            if ( reciever_type_id_with_env.type_id != type_id_special ) {
                auto const& reciever_type
                    = parent_env->get_type_at( reciever_type_id_with_env.type_id );
                assert( reciever_type.class_env_id != environment_id_undefined );

                auto const& reciever_class_env
                    = parent_env->get_env_strong_at( reciever_type.class_env_id );
                assert( reciever_class_env != nullptr );

                // TODO: check attributes

                std::cout << (const_environment_base_ptr)(reciever_class_env) << std::endl;

                // ========================================
                //
                auto const& t_env
                    = reciever_class_env->find_on_env( e->selector_id_ );
                if ( t_env == nullptr ) {
                    // try to make instance



                    // compilation error
                    assert( false && "[[CE]] identifier was not found..." );
                }

                // ========================================
                auto const& nested
                    = reciever_type_id_with_env.nest != nullptr
                    ? reciever_type_id_with_env.nest
                    : std::make_shared<std::vector<type_id_with_env>>()
                    ;
                nested->push_back( reciever_type_id_with_env );

                // ========================================
                switch( t_env->get_symbol_kind() ) {
                case kind::type_value::e_variable:
                {
                    auto const& variable_env
                        = std::static_pointer_cast<variable_symbol_environment>( t_env );
                    
                    // memoize
                    std::cout << "memoed" << std::endl;
                    variable_env->connect_from_ast( e );

                    return {
                        variable_env->get_type_id(),
                        variable_env,
                        nested
                    };
                }
                
                case kind::type_value::e_parameter_wrapper:
                    switch( std::static_pointer_cast<has_parameter_environment_base>( t_env )->get_inner_symbol_kind() ) {
                    case kind::type_value::e_function:
                    {
                        return {
                            type_id_special,
                            t_env,
                            nested
                        };
                    }

                    default:
                        assert( false && "[[CE]] invalid..." );
                        break;
                    }
                    break;

                default:
                    assert( false && "[[CE]] invalid..." );
                    break;
                }

            } else {
                assert( false && "[[ICE]]" );
            }

            //
            assert( false );
            return {
                type_id_undefined,
                nullptr,
                nullptr
            };
        }



        //
        //
        // function call expression
        RILL_TV_OP( analyzer, ast::call_expression, e, env )
        {
            using namespace boost::adaptors;
            // ========================================
            //
            auto const& reciever_type_id_with_env
                = dispatch( e->reciever_, env );

            // ========================================
            // TODO: add comma operator
            // push values to context stack and evaluate type environment
            std::vector<type_id_with_env> argument_type_ids_with_envs;

            // if lhs was nested && variable, add argument as "this"
            if ( reciever_type_id_with_env.nest ) {
                if ( reciever_type_id_with_env.nest->back().target_env->get_symbol_kind() == kind::type_value::e_variable ) {
                    argument_type_ids_with_envs.push_back( reciever_type_id_with_env.nest->back() );
                }
            }

            //
            for( auto const& val : e->arguments_ )
                argument_type_ids_with_envs.push_back( dispatch( val, env ) );

            // TODO: check type_id_special
            assert(
                std::count_if(
                    argument_type_ids_with_envs.cbegin(),
                    argument_type_ids_with_envs.cend(), []( type_id_with_env const& t ) {
                        return t.type_id == type_id_undefined
                            || t.type_id == type_id_special;
                    }
                    ) == 0
                );
           

            // ========================================
            // TODO: divide process per function, namespace, class
            if ( reciever_type_id_with_env.type_id == type_id_special ) {
                // maybe function identifier
                assert( reciever_type_id_with_env.target_env->get_symbol_kind() == kind::type_value::e_parameter_wrapper && "[[ICE]] not supported" );
                auto const has_parameter_env
                    = std::static_pointer_cast<has_parameter_environment_base>(
                        reciever_type_id_with_env.target_env
                        );

                if ( has_parameter_env->get_inner_symbol_kind() != kind::type_value::e_function ) {
                    // symbol type was not matched
                    assert( false );
                }

                // this environment has all functions that have same identifier
                auto generic_function_env
                    = std::static_pointer_cast<has_parameter_environment<function_symbol_environment>>( has_parameter_env );

                // generic_function_env has only one scope. should check parent environment.
                auto const& function_env = [&](){
                    std::cout << (const_environment_base_ptr)env << std::endl;

                    /// *************
                    // Now, e->reciever_ is expression...
                    // if reciever is Identifier
                    //   if Identifier is Function
                    //     normal function call
                    //   else
                    //     call operator() of this object
                    // else
                    //  call operator() of this object
                    //
                
                    auto const& f
                        = overload_solver_allow_no_entry(
                            argument_type_ids_with_envs | transformed( to_type_id_t() ),
                            generic_function_env,
                            env
                            );

                    // function is found!
                    if ( f )
                        return f;

                    // rescue phase...
                    //

                    // solve_forward_reference
                    for( auto const& incomplete_function_env : generic_function_env->get_incomplete_inners() ) {
                        assert( incomplete_function_env != nullptr );
                        std::cout << "found marked(ast AND related env) -> " << incomplete_function_env->get_id() << std::endl;

                        auto const& statement_node = incomplete_function_env->get_related_ast();
                        assert( statement_node != nullptr );

                        // to complate incomplete_funciton_env( after that, incomplete_function_env will be complete_function_env)
                        dispatch( statement_node, incomplete_function_env->get_parent_env() );
                    }


                    // retry
                    auto const& re_f = overload_solver(
                        argument_type_ids_with_envs | transformed( to_type_id_t() ),
                        generic_function_env,
                        env
                        );
                    if ( re_f )
                        return re_f;

                    
                    // may be overload failed
                    // TODO: dig environment once...

                    return re_f/*nullptr*/;
                }();

                // memoize called function env
                std::cout << "memoed" << std::endl;
                function_env->connect_from_ast( e );

                return {
                    function_env->get_return_type_id(),
                    function_env,
                    nullptr
                };

            } else {
                //
                assert( false && "[[ICE]] operator() is not supported");
            }



            // return retult type env of function
            //return function_env->get_return_type_environment();
        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::term_expression, e, env )
        {
            return dispatch( e->value_, env );
        }





    } // namespace semantic_analysis
} // namespace rill
