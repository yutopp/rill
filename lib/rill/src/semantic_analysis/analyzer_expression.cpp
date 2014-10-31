//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>

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
                // reciever has type, so this is member access flow
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

                //
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
                        if ( is_type_id( selector_id_type_detail->type_id )
                             && selector_id_type_detail->type_id != reciever_type_detail->type_id )
                        {
                            // a selected item has type too, so this is variable flow
                            // at least, parent's attributes are different from child. delegate it from parent
                            auto const& selector_type
                                = root_env_->get_type_at( selector_id_type_detail->type_id );
                            assert( selector_type.class_env_id != environment_id_undefined );

                            auto const new_attr
                                = delegate_parent_attributes(
                                    reciever_type.attributes,
                                    selector_type.attributes
                                    );
                            auto const new_type_id
                                = root_env_->make_type_id(
                                    selector_type.class_env_id,
                                    new_attr
                                    );

                            return type_detail_pool_->construct(
                                new_type_id,
                                selector_id_type_detail->target_env,
                                nested,
                                selector_id_type_detail->template_args
                                );

                        } else {
                            // delegate as it is
                            return type_detail_pool_->construct(
                                selector_id_type_detail->type_id,
                                selector_id_type_detail->target_env,
                                nested,
                                selector_id_type_detail->template_args
                                );
                        }

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

                std::cout
                    << "element selection: " << debug_string( new_selector_id_type_detail->target_env->get_symbol_kind() ) << std::endl
                    << "type_id: " << new_selector_id_type_detail->type_id << std::endl;

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
//                assert( false && "..." );

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

                    // set_env has multiple environments that have same name to solve overload
                    auto const& set_env = cast_to<multiple_set_environment>( reciever_type_detail->target_env );
                    assert( set_env != nullptr );

                    if ( set_env->get_representation_kind() != kind::type_value::e_function ) {
                        // symbol type was not matched
                        assert( false );
                    }

                    auto const& function_env
                        = solve_function_overload(
                            set_env,                                // overload set
                            argument_type_details,                  // type detailes of arguments
                            reciever_type_detail->template_args,    // template arguments
                            env
                            );
                    assert( function_env != nullptr );

                    // memoize called function env
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
                auto const& ty = root_env_->get_type_at( reciever_type_detail->type_id );
                auto const& c_env = std::static_pointer_cast<class_symbol_environment const>(
                    root_env_->get_env_strong_at( ty.class_env_id)
                    );

                // TODO: fix
                if ( c_env->get_base_name() == "type" ) {
                    // constructor
                    type_detail_ptr b_ty_d = nullptr;

                    resolve_type(
                        ast::helper::make_id_expression( e->reciever_ ),
                        attribute::holder_kind::k_val,     // TODO: fix
                        env,
                        [&]( type_detail_ptr const& return_ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            std::cout << "CONSTRUCTING type >> " << class_env->get_base_name() << std::endl;

                            // find constructor
                            auto const& multiset_env = cast_to<multiple_set_environment>( class_env->find_on_env( "ctor" ) );
                            assert( multiset_env->get_representation_kind() == kind::type_value::e_function );

                            // add implicit this parameter
                            std::vector<type_detail_ptr> argument_type_details_with_this( argument_type_details.size() + 1 );
                            argument_type_details_with_this[0]
                                = type_detail_factory_->change_attributes( return_ty_d, attribute::modifiability_kind::k_mutable );
                            std::copy(
                                argument_type_details.cbegin(),
                                argument_type_details.cend(),
                                argument_type_details_with_this.begin() + 1
                                );

                            auto const& function_env
                                = solve_function_overload(
                                    multiset_env,                           // overload set
                                    argument_type_details_with_this,        // type detailes of arguments
                                    nullptr,                                // template arguments
                                    class_env
                                    );
                            assert( function_env != nullptr );
                            function_env->connect_from_ast( e );

                            // TODO: fix
                            function_env->mark_as_initialize_function();

                            b_ty_d = type_detail_pool_->construct(
                                return_ty_d->type_id,
                                function_env,
                                nullptr
                                );

                            // substitute expression
                            auto substituted_ast = std::static_pointer_cast<ast::expression>(
                                std::make_shared<ast::evaluated_type_expression>( return_ty_d->type_id )
                                );
                            e->reciever_.swap( substituted_ast );
                        });

                    assert( b_ty_d != nullptr );
                    return bind_type( e, b_ty_d );

                } else {
                    // operator() invocation
                    assert( false && "[[ICE]] operator() is not supported");
                }
            }
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::id_expression, e, env )
        {
            return bind_type(
                e,
                dispatch( e->expression_, env )
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
