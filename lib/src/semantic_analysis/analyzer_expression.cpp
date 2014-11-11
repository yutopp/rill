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
            debug_out << "op_call_expr" << std::endl;

            // make argument types id list
            auto const& argument_type_details
                = evaluate_invocation_args(
                    { std::ref( e->lhs_ ), std::ref( e->rhs_ ) },
                    parent_env
                    );

            return call_suitable_binary_op(
                e->op_,
                e,
                argument_type_details,
                parent_env,
                true
                );
        }


        RILL_VISITOR_OP( analyzer, ast::unary_operator_expression, e, parent_env )
        {
            debug_out << "op_call_unary_expr" << std::endl;

            // make argument types id list
            auto const& argument_type_details
                = evaluate_invocation_args(
                    { std::ref( e->src ) },
                    parent_env
                    );

            return call_suitable_unary_op(
                e->op,
                e->is_prefix,
                e,
                argument_type_details,
                parent_env,
                true
                );
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
                    = g_env_->get_type_at( rhs_ty_d->type_id );
                auto const& rhs_c_env
                    = std::static_pointer_cast<class_symbol_environment>(
                        g_env_->get_env_at_as_strong_ref( rhs_ty.class_env_id )
                        );
                assert( rhs_c_env != nullptr );

                //
                auto const& lhs_ty
                    = g_env_->get_type_at( lhs_ty_d->type_id );
                auto const& lhs_c_env
                    = std::static_pointer_cast<class_symbol_environment>(
                        g_env_->get_env_at_as_strong_ref( lhs_ty.class_env_id )
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
                        = g_env_->get_type_at( inner_ty_id );
                    auto const& inner_c_env
                        = g_env_->get_env_at_as_strong_ref( inner_ty.class_env_id );

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

            // unreachable();
            return nullptr;
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
                //
                auto const& new_selector_id_type_detail
                    = select_member_element_universal(
                        e->selector_id_,
                        reciever_type_detail,
                        parent_env
                        );
                if ( new_selector_id_type_detail == nullptr ) {
                    assert( false && "[error] identifier not found" );
                }

                // memoize
                new_selector_id_type_detail->target_env->connect_from_ast( e );

                debug_out
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

            debug_out << "call_expr" << std::endl;

            // ========================================
            // reciever will be "function symbol", "functor object", etc...
            auto const& reciever_type_detail
                = dispatch( e->reciever_, env );

            // ========================================
            // TODO: add comma operator
            // push values to context stack and evaluate type environment
            auto const& argument_type_details
                = evaluate_invocation_args( reciever_type_detail, e->arguments_, env );

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
                    return call_function(
                        reciever_type_detail,
                        argument_type_details,
                        e,
                        env
                        );

                } else {
                    assert( false );
                }

            } else {
                auto const& ty = g_env_->get_type_at( reciever_type_detail->type_id );
                auto const& c_env = std::static_pointer_cast<class_symbol_environment const>(
                    g_env_->get_env_at_as_strong_ref( ty.class_env_id)
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
                            debug_out << "CONSTRUCTING type >> " << class_env->get_base_name() << std::endl;

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

                            auto const eval_mode = [&]() {
                                if ( function_env->has_attribute( attribute::decl::k_onlymeta ) ) {
                                    return type_detail::evaluate_mode::k_only_compiletime;
                                } else {
                                    return type_detail::evaluate_mode::k_everytime;
                                }
                            }();

                            b_ty_d = type_detail_pool_->construct(
                                return_ty_d->type_id,
                                function_env,
                                nullptr,    // nullptr
                                nullptr,    // nullptr
                                true,
                                eval_mode
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

            // unreacheble();
            return nullptr;
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


        //
        RILL_VISITOR_OP( analyzer, ast::evaluated_type_expression, e, env )
        {
            // this value contains "type_id", so the type of this expression is "type"
            auto const& type_class_env = get_primitive_class_env( "type" );
            auto const& type_type_id
                = g_env_->make_type_id( type_class_env, attribute::make_default_type_attributes() );

            return bind_type(
                e,
                type_detail_pool_->construct(
                    type_type_id,
                    type_class_env
                    )
                );
        }



    } // namespace semantic_analysis
} // namespace rill
