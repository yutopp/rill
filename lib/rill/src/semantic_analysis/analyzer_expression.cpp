//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/analyzer/identifier_solver.hpp>

#include <rill/environment/environment.hpp>

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
                return c->type_id;
            }
        };


        //
        RILL_TV_OP( analyzer, ast::binary_operator_expression, e, env )
        {
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
        }


        //
        // lhs[rhs]
        //
        RILL_TV_OP( analyzer, ast::subscrpting_expression, e, parent_env )
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
        // reciever.element
        //
        RILL_TV_OP( analyzer, ast::element_selector_expression, e, parent_env )
        {
            //
            // expr: A.B
            // A is reciever
            // B is element
            //


            // ========================================
            // eval lhs
            auto const& reciever_type_detail
                = dispatch( e->reciever_, parent_env );

            // ========================================
            // eval rhs
            // TODO: support function, class, namespace...
            // "A.B" will be passed when the value "A" is type_id symbol(currently, only variable type has type_id)
            if ( is_type_id( reciever_type_detail->type_id ) ) {
                auto const& reciever_type
                    = parent_env->get_type_at( reciever_type_detail->type_id );
                assert( reciever_type.class_env_id != environment_id_undefined );

                auto const& reciever_class_env
                    = parent_env->get_env_strong_at( reciever_type.class_env_id );
                assert( reciever_class_env != nullptr );

                // TODO: check attributes

                std::cout << (const_environment_base_ptr)(reciever_class_env) << std::endl;

                // ========================================
                //
                auto const& nested
                    = reciever_type_detail->nest != nullptr
                    ? reciever_type_detail->nest
                    : std::make_shared<type_detail::nest_type>()
                    ;
                assert( nested != nullptr );
                nested->push_back( reciever_type_detail );


                // ========================================
                //
                // use solve_identifier directly instead of doing dispatch
                // FIXME:
                auto const& selector_id_type_detail
                    = e->selector_id_->is_template()
                    ? solve_identifier(
                        this,
                        std::static_pointer_cast<ast::template_instance_value const>( e->selector_id_ ),
                        reciever_class_env,
                        true
                        )
                    : solve_identifier(
                        this,
                        std::static_pointer_cast<ast::identifier_value const>( e->selector_id_ ),
                        reciever_class_env,
                        true
                        )
                    ;

                // memoize
#if 0
                std::cout << "memoed: variable, type id = "
                          << selector_id_type_detail->target_env->get_type_id()
                          << " / "
                          << selector_id_type_detail->target_env->mangled_name()
                          << std::endl;
#endif
                selector_id_type_detail->target_env->connect_from_ast( e );

                std::cout << "element delection" << debug_string( selector_id_type_detail->target_env->get_symbol_kind() ) << std::endl;

                return bind_type(
                    e,
                    type_detail_pool_->construct(
                        selector_id_type_detail->type_id,
                        selector_id_type_detail->target_env,
                        nested,
                        selector_id_type_detail->template_args
                        )
                );

            } else {
                assert( false && "[[ICE]]" );
            }

            //
            assert( false );
            return bind_type(
                e,
                type_detail_pool_->construct(
                    type_id_undefined,
                    nullptr,
                    nullptr,
                    nullptr
                    )
            );
        }



        //
        //
        // function call expression
        RILL_TV_OP( analyzer, ast::call_expression, e, env )
        {
            using namespace boost::adaptors;

            std::cout << "call_expr" << std::endl;

            // ========================================
            //
            auto const& reciever_type_detail
                = dispatch( e->reciever_, env );

            // ========================================
            // TODO: add comma operator
            // push values to context stack and evaluate type environment
            std::vector<type_detail_ptr> argument_type_details;

            // if lhs was nested && variable, add argument as "this"
            if ( reciever_type_detail->nest ) {
                // TODO: change kind check to Callable check. Ex (1+3).operator+(6) should be callable, but can not call it now.
                if ( reciever_type_detail->nest->back()->target_env->get_symbol_kind() == kind::type_value::e_variable ) {
                    argument_type_details.push_back( reciever_type_detail->nest->back() );
                }
            }

            //
            for( auto const& val : e->arguments_ )
                argument_type_details.push_back( dispatch( val, env ) );

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

                    // FIXME: reciever_type_detail->target_env will be e_parameter_wrapper(normal) OR e_template_set(template)
                    // How do we convert template to normal,and vice versa.


                    if ( reciever_type_detail->template_args != nullptr ) {

                        // maybe function identifier
                        assert( reciever_type_detail->target_env->get_symbol_kind() == kind::type_value::e_template_set && "[[ICE]] not supported" );
                        auto const& template_set_env
                            = std::static_pointer_cast<template_set_environment>(
                                reciever_type_detail->target_env
                                );

                        if ( template_set_env->get_inner_env_symbol_kind() != kind::type_value::e_function ) {
                            // symbol type was not matched
                            assert( false );
                        }

                        // generic_function_env has only one scope. should check parent environment.
                        auto const& function_env = [&](){
                            std::cout << (const_environment_base_ptr)env << std::endl;

                            // TODO: evaluate reciever_type_detail->template_args

                            auto const& f
                            = overload_solver_allow_no_entry_with_template(
                                this,
                                reciever_type_detail->template_args,
                                argument_type_details,
                                template_set_env,
                                env
                                );

                            // function is found!
                            if ( f )
                                return f;

                            // rescue phase...
                            //

                            // solve_forward_reference
                            for( auto const& template_env : template_set_env->get_candidates() ) {
                                assert( template_env != nullptr );
                                std::cout << "template::: found marked(ast AND related env) -> " << template_env->get_id() << std::endl;

                                auto const& template_node = template_env->get_related_ast();
                                assert( template_node != nullptr );

                                auto const& function_node
                                    = std::static_pointer_cast<ast::template_statement const>( template_node )->get_inner_statement();
                                assert( function_node != nullptr );
                                // to complate incomplete_funciton_env( after that, incomplete_function_env will be complete_function_env)
                                //dispatch( statement_node, incomplete_function_env->get_parent_env() );
                            }

#if 0
                            // retry
                            auto const& re_f = overload_solver(
                                argument_type_details | transformed( to_type_id_t() ),
                                generic_function_env,
                                env
                                );
                            if ( re_f )
                                return re_f;

#endif
                            // may be overload failed
                            // TODO: dig environment once...

                            assert( false && "[ice] reached" );

                            return std::shared_ptr<function_symbol_environment>()/*DUMMY*/;
                        }();

                        assert( function_env != nullptr );

                        // memoize called function env
                        std::cout << "memoed template" << std::endl;
                        function_env->connect_from_ast( e );
                        //function_env->connect_to_ast( e );

                        std::cout << "connected template" << std::endl;

                        return bind_type(
                            e,
                            type_detail_pool_->construct(
                                function_env->get_return_type_id(),
                                function_env,
                                nullptr
                                )
                            );


                    } else {

                        // maybe function identifier
                        assert( reciever_type_detail->target_env->get_symbol_kind() == kind::type_value::e_parameter_wrapper && "[[ICE]] not supported" );
                        auto const& has_parameter_env
                            = std::static_pointer_cast<has_parameter_environment_base>(
                                reciever_type_detail->target_env
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



                            auto const& f
                            = overload_solver_allow_no_entry(
                                argument_type_details | transformed( to_type_id_t() ),
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
                                std::cout << "incomplete::: found marked(ast AND related env) -> " << incomplete_function_env->get_id() << std::endl;

                                auto const& statement_node = incomplete_function_env->get_related_ast();
                                assert( statement_node != nullptr );

                                // to complate incomplete_funciton_env( after that, incomplete_function_env will be complete_function_env)
                                dispatch( statement_node, incomplete_function_env->get_parent_env() );
                            }


                            // retry
                            auto const& re_f = overload_solver(
                                argument_type_details | transformed( to_type_id_t() ),
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
                        std::cout << "memoed function call!!!" << std::endl;
                        function_env->connect_from_ast( e );

                        return bind_type(
                            e,
                            type_detail_pool_->construct(
                                function_env->get_return_type_id(),
                                function_env,
                                nullptr
                                )
                            );
                    }
                } else {
                    assert( false );
                }

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
        RILL_TV_OP( analyzer, ast::type_expression, e, env )
        {
            return bind_type(
                e,
                dispatch( e->type_, env )
                );
        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::term_expression, e, env )
        {
            return bind_type(
                e,
                dispatch( e->value_, env )
                );
        }





    } // namespace semantic_analysis
} // namespace rill
