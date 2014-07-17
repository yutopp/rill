//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/analyzer/identifier_solver.hpp>
#include <rill/semantic_analysis/analyzer/function_solver.hpp>

#include <rill/environment/environment.hpp>

#include <rill/ast/ast.hpp>

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
                return c->type_id;
            }
        };


        //
        RILL_VISITOR_OP( analyzer, ast::binary_operator_expression, e, parent_env )
        {
            std::cout << "op_call_expr" << std::endl;

            // check lhs(reciever)
            auto const& lhs_t_detail
                = dispatch( e->lhs_, parent_env );
            // check rhs
            auto const& rhs_t_detail
                = dispatch( e->rhs_, parent_env );

            // make argument types id list
            std::vector<type_detail_ptr> const& argument_type_details
                = { lhs_t_detail, rhs_t_detail };


            // TODO: check type_id_special
            assert(
                std::count_if(
                    argument_type_details.cbegin(),
                    argument_type_details.cend(), []( type_detail_ptr const& t ) {
                        return t == nullptr || t->type_id == type_id_undefined || is_nontype_id( t->type_id );
                    } ) == 0
                );



            // 1. call lhs.operator( rhs );
            // 2. call operator( lhs, rhs );

            // 1
            // solve_identifier( e->op_, lhs_t_detail->target_env )

            // 2
            auto const& callee_function_type_detail
                = solve_identifier( e->op_, parent_env );

            //
            if ( is_nontype_id( callee_function_type_detail->type_id ) ) {
                // reciever must be function
                if ( callee_function_type_detail->type_id == (type_id_t)type_id_nontype::e_function ) {
                    std::cout << "-> " << debug_string( callee_function_type_detail->target_env->get_symbol_kind() ) << std::endl;

                    auto const& set_env = cast_to<multiple_set_environment>( callee_function_type_detail->target_env );
                    assert( set_env != nullptr );

                    if ( set_env->get_representation_kind() != kind::type_value::e_function ) {
                        // symbol type was not matched
                        assert( false );
                    }

                    auto const& function_env
                        = solve_function_overload(
                            set_env,
                            argument_type_details,                      // type detailes of arguments
                            callee_function_type_detail->template_args, // template arguments
                            parent_env
                            );
                    assert( function_env != nullptr );

                    // memoize called function env
                    std::cout << "memoed template" << std::endl;
                    function_env->connect_from_ast( e );
                    //function_env->connect_to_ast( e );

                    return bind_type(
                        e,
                        type_detail_pool_->construct(
                            function_env->get_return_type_id(),
                            function_env,
                            nullptr
                            )
                        );

                } else {
                    assert( false && "[Error]" );
                }

            } else {
                assert( false && "[Error]" );
            }


            assert(false);
#if 0
            using namespace boost::adaptors;

            // check type environment
            auto const& rhs_t_detail
                = dispatch( e->rhs_, env );
            auto const& lhs_t_detail
                = dispatch( e->lhs_, env );


            // make argument types id list
            std::vector<type_detail_ptr> const& argument_type_details
                = { lhs_t_detail, rhs_t_detail };

            // TODO: check type_id_special
            assert(
                std::count_if(
                    argument_type_details.cbegin(),
                    argument_type_details.cend(), []( type_detail_ptr const& t ) {
                        return t->type_id == type_id_undefined
                            || is_nontype_id( t->type_id );
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
                    argument_type_details | transformed( to_type_id_t() ),
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
            return bind_type(
                e,
                type_detail_pool_->construct(
                    function_env->get_return_type_id(),
                    function_env
                    )
            );
#endif
            return nullptr;
        }


        //
        // lhs[rhs]
        //
        RILL_VISITOR_OP( analyzer, ast::subscrpting_expression, e, parent_env )
        {
            if ( e->rhs_ ) {
                // has capseled value
                // lhs[rhs]

                // evaluate rhs first, then lhs
                //
                auto const& rhs_ty_d
                    = dispatch( *e->rhs_, parent_env );
                //
                auto const& lhs_ty_d
                    = dispatch( e->lhs_, parent_env );

                //
                auto const& rhs_ty
                    = root_env_->get_type_at( rhs_ty_d->type_id );
                auto const& rhs_c_env
                    = std::static_pointer_cast<class_symbol_environment>(
                        root_env_->get_env_strong_at( rhs_ty.class_env_id )
                        );
                assert( rhs_c_env != nullptr );

                //
                auto const& lhs_ty
                    = root_env_->get_type_at( lhs_ty_d->type_id );
                auto const& lhs_c_env
                    = std::static_pointer_cast<class_symbol_environment>(
                        root_env_->get_env_strong_at( lhs_ty.class_env_id )
                        );
                assert( lhs_c_env != nullptr );
                // if lhs is "array", treat as builtin array type...
                if ( lhs_c_env->is_array() ) {
                    // this is builtin ARRAY!

                    // if array, AST connects class environment
                    lhs_c_env->connect_from_ast( e );

                    auto const& inner_ty_id
                        = lhs_c_env->get_array_detail()->inner_type_id;
                    auto const& inner_ty
                        = root_env_->get_type_at( inner_ty_id );
                    auto const& inner_c_env
                        = root_env_->get_env_strong_at( inner_ty.class_env_id );

                    //
                    return bind_type(
                        e,
                        /* incomplete...? */
                        type_detail_pool_->construct(
                            inner_ty_id,
                            inner_c_env
                            )
                        );

                } else {
                    // lhs[rhs]
                    // TODO: call operator[expr]
                    assert( false && "[ice] not supported..." );
                }

            } else {
                // lhs[]
                // TODO: call operator[]
                assert( false && "[ice] not supported..." );
            }
        }



        //
        // check forms like "reciever.element"
        //
        RILL_VISITOR_OP( analyzer, ast::element_selector_expression, e, parent_env )
        {
            //
            // expr: "Expr.B"
            // Expr is reciever as lhs
            // B is element as rhs
            //

            // ========================================
            // eval lhs
            auto const& reciever_type_detail
                = dispatch( e->reciever_, parent_env );
            assert( reciever_type_detail != nullptr );

            // ========================================
            // eval rhs
            // TODO: support function, class, namespace...
            // "Expr.B" will be passed when the value "Expr" is type_id symbol(currently, only variable type has type_id)
            if ( is_type_id( reciever_type_detail->type_id ) ) {
                auto const& reciever_type
                    = parent_env->get_type_at( reciever_type_detail->type_id );
                assert( reciever_type.class_env_id != environment_id_undefined );

                auto const& reciever_class_env
                    = parent_env->get_env_at_as_strong_ref( reciever_type.class_env_id );
                assert( reciever_class_env != nullptr );

                // ========================================
                // nest data
                auto const& nested
                    = reciever_type_detail->nest != nullptr
                    ? reciever_type_detail->nest
                    : std::make_shared<type_detail::nest_type>()
                    ;
                assert( nested != nullptr );

                // chain type_detail data
                // <- old [first evaled reciever type, second evaled..., ..., last evaled reciever type] -> new
                nested->push_back( reciever_type_detail );

                // ========================================
                // use solve_identifier directly instead of doing dispatch
                // FIXME:


                auto const& new_selector_id_type_detail = [&]() {
                    auto const& selector_id_type_detail
                        = e->selector_id_->is_template()
                            ? solve_identifier(
                                std::static_pointer_cast<ast::template_instance_value const>( e->selector_id_ ),
                                reciever_class_env,
                                false
                                )
                            : solve_identifier(
                                std::static_pointer_cast<ast::identifier_value const>( e->selector_id_ ),
                                reciever_class_env,
                                false
                                )
                        ;

                    if ( selector_id_type_detail != nullptr ) {
                        return type_detail_pool_->construct(
                            selector_id_type_detail->type_id,
                            selector_id_type_detail->target_env,
                            nested,
                            selector_id_type_detail->template_args
                            );

                    } else {
                        // identifier was not found, but it will be reused by UFCS, so make temporary type data

                        assert( false );

                        return type_detail_pool_->construct(
                            selector_id_type_detail->type_id,
                            parent_env,
                            nested,
                            selector_id_type_detail->template_args
                            );
                    }
                } ();


                // memoize
                new_selector_id_type_detail->target_env->connect_from_ast( e );

                std::cout << "element delection" << debug_string( new_selector_id_type_detail->target_env->get_symbol_kind() ) << std::endl;

                return bind_type( e, new_selector_id_type_detail );

            } else {
                assert( false && "[[ICE]]" );
            }

            //
            assert( false );
            return nullptr;
        }



        //
        //
        // function call expression
        RILL_VISITOR_OP( analyzer, ast::call_expression, e, env/*parent_env*/ )
        {
            using namespace boost::adaptors;

            std::cout << "call_expr" << std::endl;

            // ========================================
            // reciever will be "function symbol", "functor object", etc...
            auto const& reciever_type_detail
                = dispatch( e->reciever_, env );

            // ========================================
            // TODO: add comma operator
            // push values to context stack and evaluate type environment
            std::vector<type_detail_ptr> argument_type_details;

            if ( reciever_type_detail->nest ) {
                // 2014/5/14, selected call has no specification...
                assert( false && "..." );

                // if lhs was nested && variable, add argument as "this"
                // TODO: change kind check to Callable check. Ex (1+3).operator+(6) should be callable, but can not call it now.
                if ( reciever_type_detail->nest->back()->target_env->get_symbol_kind() == kind::type_value::e_variable ) {
                    argument_type_details.push_back( reciever_type_detail->nest->back() );
                }
            }



            // make argument type list
            for( auto const& val : e->arguments_ )
                argument_type_details.push_back( dispatch( val, env ) );

            // TODO: check type_id_special
            assert(
                std::count_if(
                    argument_type_details.cbegin(),
                    argument_type_details.cend(), []( type_detail_ptr const& t ) {
                        return t->type_id == type_id_undefined || is_nontype_id( t->type_id );
                    } ) == 0
                );

            /// *************
            // TODO:
            // Now, e->reciever_ is expression...
            // if reciever is Identifier
            //   if Identifier is Function
            //     normal function call
            //   else
            //     call operator() of this object
            // else
            //  call operator() of this object
            //

            // ========================================
            // TODO: divide process per function, namespace, class
            if ( is_nontype_id( reciever_type_detail->type_id ) ) {
                // reciever must be function
                if ( reciever_type_detail->type_id == (type_id_t)type_id_nontype::e_function ) {
                    // TODO:
                    // if the reciever has template args
                    //     solve from template instantiation
                    //     if symbol not found
                    //         ERROR
                    // else
                    //     solve from normal solver
                    //     if symbol not found
                    //         solve from template instantiation
                    //         if symbol not found
                    //             ERROR

                    // NOTE: reciever_type_detail->target_env will be "multiple_set_environment"
                    std::cout << "Normal function solve" << std::endl;

                    std::cout << "-> " << debug_string( reciever_type_detail->target_env->get_symbol_kind() ) << std::endl;

                    auto const& set_env = cast_to<multiple_set_environment>( reciever_type_detail->target_env );
                    assert( set_env != nullptr );

                    if ( set_env->get_representation_kind() != kind::type_value::e_function ) {
                        // symbol type was not matched
                        assert( false );
                    }

                    auto const& function_env
                        = solve_function_overload(
                            set_env,
                            argument_type_details,                  /* type detailes of arguments */
                            reciever_type_detail->template_args,    /* template arguments */
                            env
                            );
                    assert( function_env != nullptr );

                    // memoize called function env
                    std::cout << "memoed template" << std::endl;
                    function_env->connect_from_ast( e );
                    //function_env->connect_to_ast( e );

                    return bind_type(
                        e,
                        type_detail_pool_->construct(
                            function_env->get_return_type_id(),
                            function_env,
                            nullptr
                            )
                        );

                } else {
                    assert( false );
                }

            } else {
                //
                assert( false && "[[ICE]] operator() is not supported");
            }

            assert( false );

            // return retult type env of function
            //return function_env->get_return_type_environment();
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::type_expression, e, env )
        {
            return bind_type(
                e,
                dispatch( e->type_, env )
                );
        }



        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::term_expression, e, env )
        {
            return bind_type(
                e,
                dispatch( e->value_, env )
                );
        }





    } // namespace semantic_analysis
} // namespace rill
