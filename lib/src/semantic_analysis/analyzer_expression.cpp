//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/message_code.hpp>

#include <rill/environment/environment.hpp>

#include <rill/ast/ast.hpp>

#include <rill/utility/tie.hpp>

#include <vector>
#include <boost/range/adaptor/transformed.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        //
        RILL_VISITOR_OP( analyzer, ast::binary_operator_expression, e, parent_env )
        {
            rill_dout << "op_call_expr" << std::endl;

            // make argument types id list
            auto const& argument_type_details
                = evaluate_invocation_args(
                    { std::ref( e->lhs_ ), std::ref( e->rhs_ ) },
                    parent_env
                    );

            auto const& op_name = make_binary_op_name( e->op_ );
            auto const ty_d = call_suitable_operator(
                op_name,
                e,
                argument_type_details,
                parent_env,
                true
                );
            if ( ty_d == nullptr ) {
                // compilation error...
                rill_dout << "% name : "
                          << op_name->get_inner_symbol()->to_native_string() << std::endl;
                assert( false && "[Error] identifier was not found." );
            }

            return ty_d;
        }


        RILL_VISITOR_OP( analyzer, ast::unary_operator_expression, e, parent_env )
        {
            rill_dout << "op_call_unary_expr" << std::endl;

            // make argument types id list
            auto const& argument_type_details
                = evaluate_invocation_args(
                    { std::ref( e->src ) },
                    parent_env
                    );

            auto const& op_name = make_unary_op_name( e->op, e->is_prefix );
            auto const ty_d = call_suitable_operator(
                op_name,
                e,
                argument_type_details,
                parent_env,
                true
                );
            if ( ty_d == nullptr ) {
                // compilation error...
                rill_dout << "% name : "
                          << op_name->get_inner_symbol()->to_native_string() << std::endl;
                assert( false && "[Error] identifier was not found." );
            }

            return ty_d;
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

                rill_dout
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
        RILL_VISITOR_OP( analyzer, ast::call_expression, e, parent_env )
        {
            rill_dout << "call_expr" << std::endl;

            // ========================================
            // reciever will be "function symbol", "functor object", etc...
            auto const& reciever_type_detail
                = dispatch( e->reciever_, parent_env );

            // ========================================
            // TODO: add comma operator
            // push values to context stack and evaluate type environment
            auto const& argument_type_details
                = evaluate_invocation_args( reciever_type_detail, e->arguments_, parent_env );

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
                        parent_env
                        );

                } else {
                    assert( false );
                }

            } else {
                auto const& ty = g_env_->get_type_at( reciever_type_detail->type_id );
                auto const& c_env = std::static_pointer_cast<class_symbol_environment const>(
                    g_env_->get_env_at_as_strong_ref( ty.class_env_id )
                    );

                // TODO: fix
                if ( c_env->get_base_name() == "type" ) {
                    // constructor
                    return call_constructor( e, argument_type_details, parent_env );

                } else {
                    // operator() invocation
                    // TODO: fix, not binary operator
                    static auto const call_op = ast::make_identifier( "()" );
                    auto const& op_name = make_binary_op_name( call_op );

                    std::vector<type_detail_ptr> argument_type_details_with_this(
                        argument_type_details.size() + 1
                        );
                    argument_type_details_with_this[0]
                        = reciever_type_detail;
                    std::copy(
                        argument_type_details.cbegin(),
                        argument_type_details.cend(),
                        argument_type_details_with_this.begin() + 1
                        );

                    auto const ty_d = call_suitable_operator(
                        op_name,
                        e,
                        argument_type_details_with_this,
                        parent_env,
                        false // without universal searching
                        );

                    if ( ty_d == nullptr ) {
                        // compilation error...
                        rill_dout << "% name : "
                                  << op_name->get_inner_symbol()->to_native_string() << std::endl;
                        rill_ice( "[Error] identifier was not found." );
                    }

                    return ty_d;
                }
            }

            // unreacheble();
            return nullptr;
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::id_expression, e, parent_env )
        {
            return bind_type(
                e,
                dispatch( e->expression_, parent_env )
                );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::dereference_expression, e, parent_env )
        {
            rill_dout << "dereference_expr" << std::endl;

            // make argument types id list
            auto const& argument_type_details
                = evaluate_invocation_args(
                    { std::ref( e->reciever ) },
                    parent_env
                    );
            assert( argument_type_details.size() == 1 );
            auto const& reciever_ty
                = g_env_->get_type_at( argument_type_details[0]->type_id );
            auto const& reciever_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment>(
                    reciever_ty.class_env_id
                    );
            assert( reciever_c_env != nullptr );
            if ( !reciever_c_env->is_pointer() ) {
                // non pointer object, try to call "op pre *"
                static auto const addressof_op = ast::make_identifier( "*" );
                auto const& op_name = make_unary_op_name( addressof_op, true /*prefix*/ );
                auto const ty_d = call_suitable_operator(
                    op_name,
                    e,
                    argument_type_details,
                    parent_env,
                    true
                    );
                if ( ty_d != nullptr ) {
                    // found user defined operator
                    return ty_d;

                } else {
                    assert( "ice: can not dereference this type" );
                }
            }

            auto const& p_d = reciever_c_env->get_pointer_detail();
            auto const& value_type_id = p_d->inner_type_id;

            // delegate parent attributes
            // Ex, const(ptr!int) -> dereference -> const(int)
            auto const& ty = g_env_->get_type_at( value_type_id );
            auto const& attr = mask_transitively( ty.attributes, reciever_ty.attributes );

            auto new_type_id = g_env_->make_type_id( ty.class_env_id, attr );

            return bind_type(
                e,
                type_detail_pool_->construct(
                    new_type_id,
                    nullptr // unused
                    )
                );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::addressof_expression, e, parent_env )
        {
            rill_dout << "addressof_expr" << std::endl;

            // make argument types id list
            auto const& argument_type_details
                = evaluate_invocation_args(
                    { std::ref( e->reciever ) },
                    parent_env
                    );
            assert( argument_type_details.size() == 1 );
            auto const& reciever_ty
                = g_env_->get_type_at( argument_type_details[0]->type_id );
            auto const& reciever_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment>(
                    reciever_ty.class_env_id
                    );
            assert( reciever_c_env != nullptr );
            if ( !reciever_c_env->is_pointer() ) {
                // non pointer object, try to call "op pre &"
                static auto const addressof_op = ast::make_identifier( "&" );
                auto const& op_name = make_unary_op_name( addressof_op, true /*prefix*/ );
                auto const ty_d = call_suitable_operator(
                    op_name,
                    e,
                    argument_type_details,
                    parent_env,
                    true
                    );
                if ( ty_d != nullptr ) {
                    // found user defined operator
                    return ty_d;
                }
            }

            // intrinsic addressof
            auto const& ptr_ty_d = make_pointer_type(
                argument_type_details[0]->type_id,
                e,
                parent_env
                );

            return bind_type( e, ptr_ty_d );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::lambda_expression, e, parent_env )
        {
            static int id = 0;
            auto const& lambda_class_id
                = ast::make_identifier( std::string( "__lambda_" ) + std::to_string( id ) );
            int const current_id = id;
            ++id;

            if ( parent_env->find_on_env( lambda_class_id ) != nullptr ) {
                rill_ice( "" );
            }

            if ( e->template_parameters ) {
                rill_ice( "Not supported" );
#if 0
                // operator call for lambda object
                auto const& lambda_op_call_id
                    = ast::make_identifier( "%op_()" );
                auto lambda_op_call_def
                    = std::make_shared<ast::class_function_definition_statement>(
                        lambda_op_call_id,
                        e->parameters,
                        attribute::decl::k_default,
                        boost::none,
                        boost::none,
                        std::make_shared<ast::statements>(
                            e->statements
                            )
                        );
                auto template_lambda_op_call_def
                    = std::make_shared<ast::template_statement>(
                        *e->template_parameters,
                        lambda_op_call_def
                        );

                // TODO: make cut block environment
                // Create Class AST
                auto const& ast_for_lambda_class
                    = std::make_shared<ast::class_definition_statement>(
                        lambda_class_id,
                        attribute::decl::k_default,
                        std::make_shared<ast::statements>(
                            ast::element::statement_list{
                                template_lambda_op_call_def
                            }
                            )
                        );

                // generate lambda function class
                auto const& import_base = import_bases_.top();

                auto const& report = collect_identifier( g_env_, ast_for_lambda_class, parent_env, import_base );
                if ( report->is_errored() ) {
                    import_messages( report );
                    // TODO: raise semantic error
                    assert( false );
                    return nullptr;
                }
                dispatch( ast_for_lambda_class, parent_env );


                auto const& c_env = cast_to<class_symbol_environment>(
                    g_env_->get_related_env_by_ast_ptr( ast_for_lambda_class )
                    );
                assert( c_env != nullptr );
                rill_dout << "class    -> " << c_env->get_qualified_name() << " / ptr: " << c_env << std::endl;

                ast::parameter_list parames_for_ctor;
                ast::expression_list args_for_ctor;
                ast::element::class_variable_initializers initializer({});

                // 'constructor' for lambda object
                {
                    auto const& lambda_ctor_id
                        = ast::make_identifier( "ctor" );
                    auto lambda_ctor_def
                        = std::make_shared<ast::class_function_definition_statement>(
                            lambda_ctor_id,
                            std::move( parames_for_ctor ),
                            e->decl_attr,
                            std::move( initializer ),
                            e->return_type,
                            std::make_shared<ast::statements>(
                                ast::element::statement_list{}
                                )
                        );

                    auto const& report
                        = collect_identifier(
                            g_env_,
                            lambda_ctor_def,
                            c_env,
                            import_base
                            );
                    if ( report->is_errored() ) {
                        import_messages( report );
                        // TODO: raise semantic error
                        assert( false );
                        return nullptr;
                    }
                    dispatch( lambda_ctor_def, c_env );
                }

                auto const& argument_type_details
                    = evaluate_invocation_args( nullptr, args_for_ctor, parent_env );

                // create lambda object
                auto reciever = std::make_shared<ast::term_expression>( lambda_class_id );
                auto const& call_expr
                    = std::make_shared<ast::call_expression>(
                        std::move( reciever ),
                        std::move( args_for_ctor )
                        );

                // memoize call expression to lambda ast
                e->call_expr = call_expr;

                // construct lambda object
                auto return_ty_d
                    = call_constructor( call_expr, argument_type_details, parent_env );

                return bind_type( e, return_ty_d );
#endif
            }


            // operator call for lambda object
            auto const& lambda_op_call_id
                = ast::make_identifier( "%op_()" );
            auto lambda_op_call_def
                = std::make_shared<ast::class_function_definition_statement>(
                    lambda_op_call_id,
                    e->parameters,
                    attribute::decl::k_default,
                    boost::none,
                    boost::none,
                    std::make_shared<ast::statements>(
                        e->statements
                        )
                    );

            // TODO: make cut block environment
            // Create Class AST
            auto const& ast_for_lambda_class
                = std::make_shared<ast::class_definition_statement>(
                    lambda_class_id,
                    boost::none,    // base class
                    boost::none,    // traits
                    attribute::decl::k_default,
                    std::make_shared<ast::statements>(
                        ast::element::statement_list{
                            lambda_op_call_def
                        }
                        )
                    );

            // generate lambda function class
            auto const& import_base = import_bases_.top();

            auto const& report = collect_identifier( g_env_, ast_for_lambda_class, parent_env, import_base );
            if ( report->is_errored() ) {
                import_messages( report );
                // TODO: raise semantic error
                assert( false );
                return nullptr;
            }
            dispatch( ast_for_lambda_class, parent_env );


            // CAPTURE!
            rill_dout << "CAPTURE" << std::endl;
            auto const& c_env = cast_to<class_symbol_environment>(
                g_env_->get_related_env_by_ast_ptr( ast_for_lambda_class )
                );
            assert( c_env != nullptr );
            rill_dout << "class    -> " << c_env->get_qualified_name() << " / ptr: " << c_env << std::endl;

            auto const& f_env = cast_to<function_symbol_environment>(
                g_env_->get_related_env_by_ast_ptr( lambda_op_call_def )
                );
            assert( f_env != nullptr );
            rill_dout << "function -> " << f_env->get_mangled_name() << " / ptr: " << f_env << std::endl
                      << "captured num: " << f_env->get_outer_referenced_asts().size() << std::endl;



            std::size_t index = 0;
            ast::parameter_list parames_for_ctor;
            ast::expression_list args_for_ctor;
            ast::element::class_variable_initializers initializer({});
            std::unordered_map<ast::native_string_t, std::size_t> captured;

            for( auto&& ex_ast : f_env->get_outer_referenced_asts() ) {
                rill_dregion {
                    rill_dout << "=====================  outer ast ptr " << ex_ast << " / ID: " << ex_ast->get_id() << std::endl;
                    ex_ast->dump( std::cout );
                }

                auto const& ex_tid = g_env_->get_related_type_id_from_ast_ptr( ex_ast );
                if ( !is_type_id( ex_tid ) ) {
                    continue;
                }

                auto const& ex_type = g_env_->get_type_at( ex_tid );
                auto const& ex_c_env = g_env_->get_env_at_as_strong_ref<class_symbol_environment>( ex_type.class_env_id );

                // TODO: fix
                if ( ex_c_env->has_attribute( attribute::decl::k_onlymeta ) ) {
                    continue;
                }

                //
                auto const id = ex_ast->get_id();

                // cache
                auto const& name
                    = ex_ast->get_inner_symbol()->to_native_string();

                auto const it = captured.find( name );
                if ( it != captured.cend() ) {
                    // continue;
                    // !!: replace ast node
                    assert( !ex_ast->parent_expression.expired() );
                    auto expr = ex_ast->parent_expression.lock();
                    if ( captured_value_ids_.count( expr->value_->get_id() ) == 0 ) {
                        expr->value_
                            = std::make_shared<ast::captured_value>(
                                it->second,
                                f_env->get_id()
                                );
                        expr->value_->parent_expression = expr;

                        captured_type_details_.emplace(
                            expr->value_->get_id(),
                            captured_type_details_.at( id )
                            );

                        captured_value_ids_.emplace( expr->value_->get_id() );
                    }
                    continue;
                }
                captured.emplace( name, index );


                //
                // create places for captured variables
                //
                auto vd = ast::variable_declaration{
                    ex_type.attributes.quality,
                    ast::variable_declaration_unit{
                        ex_ast,
                        ast::value_initializer_unit{
                            std::make_shared<ast::id_expression>(
                                std::make_shared<ast::evaluated_type_expression>(
                                    ex_tid
                                    )
                                ),
                            boost::none
                        }
                    }
                };
                auto const& captured_v_decl
                    = std::make_shared<ast::class_variable_declaration_statement>(
                        std::move( vd )
                        );
                auto const& report
                    = collect_identifier(
                        g_env_,
                        captured_v_decl,
                        c_env,
                        import_base
                        );
                if ( report->is_errored() ) {
                    import_messages( report );
                    // TODO: raise semantic error
                    assert( false );
                    return nullptr;
                }
                dispatch( captured_v_decl, c_env );

                // append class variables
                ast_for_lambda_class->inner_->statements_.push_back( captured_v_decl );

                auto const& v_env = cast_to<variable_symbol_environment>(
                    g_env_->get_related_env_by_ast_ptr( captured_v_decl )
                    );
                assert( v_env != nullptr );
                assert( v_env->get_parent_class_env_id() == c_env->get_id() );


                //
                auto for_arg = std::make_shared<ast::term_expression>(
                    clone( ex_ast )
                    );
                for_arg->value_->parent_expression = for_arg;

                //
                auto init_name = clone( ex_ast );
                auto init_expr = std::make_shared<ast::term_expression>(
                    clone( ex_ast )
                    );
                init_expr->value_->parent_expression = init_expr;

                // !!: replace ast node
                assert( !ex_ast->parent_expression.expired() );
                auto expr = ex_ast->parent_expression.lock();
                if ( captured_value_ids_.count( expr->value_->get_id() ) == 0 ) {
                expr->value_
                    = std::make_shared<ast::captured_value>(
                        index,
                        f_env->get_id()
                        );
                expr->value_->parent_expression = expr;

                captured_type_details_.emplace(
                    expr->value_->get_id(),
                    captured_type_details_.at( id )
                    );

                captured_value_ids_.emplace( expr->value_->get_id() );
                }


                //
                //
                //
                auto pvd = ast::variable_declaration{
                    ex_type.attributes.quality,
                    ast::variable_declaration_unit{
                        ex_ast,
                        ast::value_initializer_unit{
                            std::make_shared<ast::id_expression>(
                                std::make_shared<ast::evaluated_type_expression>(
                                    ex_tid
                                    )
                                ),
                            boost::none
                        }
                    }
                };
                parames_for_ctor.emplace_back( std::move( pvd ) );


                //
                //
                //
                args_for_ctor.emplace_back( std::move( for_arg ) );


                // initializer
                // ctor() | ex_ast = arg ...
                initializer.initializers.emplace_back(
                    ast::variable_declaration_unit{
                        init_name,
                        ast::value_initializer_unit{
                            init_expr
                        }
                    }
                    );
                // link
                v_env->connect_from_ast( init_name );

                //
                ++index;
            }


            // 'constructor' for lambda object
            {
                auto const& lambda_ctor_id
                    = ast::make_identifier( "ctor" );
                auto lambda_ctor_def
                    = std::make_shared<ast::class_function_definition_statement>(
                        lambda_ctor_id,
                        std::move( parames_for_ctor ),
                        attribute::decl::k_default,
                        /*boost::none,/*/std::move( initializer ),/**/
                        boost::none,
                        std::make_shared<ast::statements>(
                            ast::element::statement_list{}
                            )
                        );

                auto const& report
                    = collect_identifier(
                        g_env_,
                        lambda_ctor_def,
                        c_env,
                        import_base
                        );
                if ( report->is_errored() ) {
                    import_messages( report );
                    // TODO: raise semantic error
                    assert( false );
                    return nullptr;
                }
                dispatch( lambda_ctor_def, c_env );
            }

            auto const& argument_type_details
                = evaluate_invocation_args( nullptr, args_for_ctor, parent_env );

            // create lambda object
            auto reciever = std::make_shared<ast::term_expression>( lambda_class_id );
            auto const& call_expr
                = std::make_shared<ast::call_expression>(
                    std::move( reciever ),
                    std::move( args_for_ctor )
                    );

            // memoize call expression to lambda ast
            e->call_expr = call_expr;

            // construct lambda object
            auto return_ty_d
                = call_constructor( call_expr, argument_type_details, parent_env );

            return bind_type( e, return_ty_d );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::term_expression, e, parent_env )
        {
            return bind_type(
                e,
                dispatch( e->value_, parent_env )
                );
        }


        //
        RILL_VISITOR_OP( analyzer, ast::evaluated_type_expression, e, parent_env )
        {
            // this value contains "type_id", so the type of this expression is "type"
            auto const& type_class_env = get_primitive_class_env( "type" );
            auto const& type_type_id
                = g_env_->make_type_id( type_class_env, attribute::make_default() );

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
