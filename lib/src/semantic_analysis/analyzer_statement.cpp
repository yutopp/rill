//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/messaging.hpp>
#include <rill/semantic_analysis/message_code.hpp>
#include <rill/semantic_analysis/make_mangled_name.hpp>

#include <rill/environment/environment.hpp>
#include <rill/environment/make_module_name.hpp>
#include <rill/behavior/intrinsic_action_holder.hpp>

#include <rill/ast/ast.hpp>
#include <rill/utility/tie.hpp>

#include <algorithm>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>

#include <boost/scope_exit.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // root of ast
        RILL_VISITOR_OP( analyzer, ast::module, s, parent_env )
        {
            auto const working_dir
                = s->fullpath.empty()
                ? fs::current_path()
                : s->fullpath.parent_path();
            working_dirs_.push( working_dir );
            if ( import_bases_.empty() ) {
                // set default import path!
                import_bases_.push( working_dir );
            }
            auto const& import_base = import_bases_.top();

            rill_dout << "working dir  : " << working_dir << std::endl
                      << "import_bases : " << import_base << std::endl;

            // to forward reference
            auto const& report = collect_identifier( g_env_, s, parent_env, import_base );
            if ( report->is_errored() ) {
                import_messages( report );
                working_dirs_.pop();
                return;
            }

            //
            auto const& module_name = make_module_name( import_base, s );

            auto module_env = g_env_->find_module( module_name );
            module_envs_.push( module_env );

            block_envs_.emplace( module_env );
            BOOST_SCOPE_EXIT_ALL(this) {
                block_envs_.pop();
            };

            if ( module_name.empty() || module_name == "basic_types" ) {
                builtin_class_envs_cache_
                    = std::make_shared<builtin_class_envs_cache>( module_env );

            } else {
                // all modules(except 'basic_types') must import 'basic_types' module.
                auto const i_decl = ast::import_decl_unit{
                    "basic_types"
                };
                import_module( i_decl, module_env );
            }

            //
            assert( s->program != nullptr );
            dispatch( s->program, module_env );
        }


        RILL_VISITOR_OP( analyzer, ast::statements, s, parent_env )
        {
            // build environment
            for( auto const& ss : s->statements_ )
                dispatch( ss, parent_env );
        }

        RILL_VISITOR_OP( analyzer, ast::import_statement, s, parent_env )
        {
            for( auto&& decl : s->module_decls ) {
                import_module( decl, parent_env );
            }
        }

        RILL_VISITOR_OP( analyzer, ast::block_statement, s, parent_env )
        {
            auto const& scope_env
                = parent_env->construct( kind::k_scope );
            scope_env->link_with_ast( s->statements_ );

            block_envs_.emplace( scope_env );
            BOOST_SCOPE_EXIT_ALL(this) {
                block_envs_.pop();
            };

            dispatch( s->statements_, scope_env );

            // delegate is_closed
            parent_env->mask_is_closed( scope_env->is_closed() );
        }

        RILL_VISITOR_OP( analyzer, ast::template_statement, s, parent_env )
        {
            // do nothing
        }

        // statement
        RILL_VISITOR_OP( analyzer, ast::expression_statement, s, parent_env )
        {
            auto const& ty_d = dispatch( s->expression_, parent_env );
            if ( ty_d->eval_mode == type_detail::evaluate_mode::k_only_compiletime ) {
                substitute_by_ctfed_node( s->expression_, ty_d, parent_env );
            }

            // TODO: check this expression is [noreturn].
            // [noreturn] means control flow will go to elsewhere. so closed.
            // parent_env->mask_is_closed( true );
        }

        //
        RILL_VISITOR_OP( analyzer, ast::return_statement, s, parent_env )
        {
            // Return Statement is valid only in Function Envirionment...
            auto const& a_env = parent_env->lookup_layer( kind::type_value::e_function );
            assert( a_env != nullptr ); // TODO: change to error_handler

            rill_dout << "Return Statement [target: f_env] ------>> " << std::endl
                      << " type: " << debug_string( a_env->get_symbol_kind() ) << std::endl
                      << " env : " << a_env << std::endl;

            auto const& callee_f_env = cast_to<function_symbol_environment>( a_env );
            assert( callee_f_env != nullptr );

            auto const& return_type_detail
                = dispatch( s->expression_, parent_env );

            assert( !is_nontype_id( return_type_detail->type_id ) && "[[CE]] this object couldn't be returned" );

            if ( callee_f_env->is_return_type_decided() ) {
                // type check
                auto const& cf_tid = callee_f_env->get_return_type_id();
                auto const& cf_ty = g_env_->get_type_at( cf_tid );

                // check if available converting from return_type_detail->type_id to cf_tid

            } else {
                // return statement that first apeared in function env
                //   sets default return type, if return type is not decided
                callee_f_env->decide_return_type( return_type_detail->type_id );
            }

            // statement return to anywhare, so this environment is closed
            parent_env->mask_is_closed( true );
        }



        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::variable_declaration_statement, s, parent_env )
        {
            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( related_env == nullptr && "[[ice]]" ); // ??

            auto const& val_decl = s->declaration_;
            // TODO: decl_unit will be unit_list
            // for( auto const& unit : val_decl.decl_unit_list ) {
            auto const& unit = val_decl.decl_unit;

            regard_variable_is_not_already_defined(
                parent_env,
                unit.name
                );

            // initial value
            auto const& iv_type_d
                = unit.init_unit.initializer
                ? dispatch( unit.init_unit.initializer, parent_env )
                : nullptr;

            // TODO: make method to determine "type"

            // unit.kind -> val or ref
            // TODO: use unit.kind( default val )

            // TODO: evaluate type || type inference || type check
            //       default( int )

            if ( unit.init_unit.type ) { // is parameter variable type specified ?
                resolve_type(
                    unit.init_unit.type,
                    val_decl.quality,
                    parent_env,
                    [&]( type_detail_ptr const& ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        if ( is_nontype_id( ty_d->type_id ) ) {
                            assert( false && "[[error]] incomplete template class was not supported" );
                        }

                        // TODO: move to elsewhere
                        if ( class_env->is_incomplete() ) {
                            //
                            auto const& class_node = class_env->get_related_ast();
                            assert( class_node != nullptr );

                            dispatch( class_node, class_env->get_parent_env() );
                            assert( class_env->is_complete() );
                        }

                        // define variable
                        auto const& variable_env
                            = parent_env->incomplete_construct(
                                kind::k_variable,
                                unit.name
                                );
                        variable_env->link_with_ast( s );
                        variable_env->complete( ty_d->type_id );

                        if ( iv_type_d != nullptr ) {
                            // type check
                            RILL_PP_TIE( level, conv_function_env,
                                         try_type_conversion(
                                             ty_d->type_id,
                                             iv_type_d->type_id,
                                             parent_env
                                             )
                                );

                            switch( level ) {
                            case function_match_level::k_exact_match:
                                rill_dout << "Exact" << std::endl;
                                break;
                            case function_match_level::k_qualifier_conv_match:
                                rill_dout << "Qual" << std::endl;
                                break;
                            case function_match_level::k_implicit_conv_match:
                                rill_dout << "Implicit" << std::endl;
                                break;
                            case function_match_level::k_no_match:
                                rill_dout << "NoMatch" << std::endl;
                                assert( false && "[[error]] This value can not be assigned(type conversion)" );
                            }
                        }
                    });

            } else {
                // type inferenced by result of evaluated "iv_type_id_and_env"
                assert( iv_type_d != nullptr );

                //
                auto const& v_env = parent_env->incomplete_construct(
                    kind::k_variable,
                    unit.name
                    );
                v_env->link_with_ast( s );
                v_env->complete( iv_type_d->type_id );
            }
        }





        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::class_variable_declaration_statement, s, parent_env )
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            // TODO: make unit of variable decl to "AST NODE".
            // for( auto const& decl : s->decls_ ) {

            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            // Forward referencable
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_variable );

            auto const& v_env = cast_to<variable_symbol_environment>( related_env );
            assert( v_env != nullptr );

            // guard double check
            if ( v_env->is_checked() )
                return;
            v_env->change_progress_to_checked();

            auto const& parent_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment>(
                    v_env->get_parent_class_env_id()
                    );
            assert( parent_c_env != nullptr );

            auto const& val_decl = s->declaration_;
            // TODO: decl_unit will be unit_list
            // for( auto const& unit : val_decl.decl_unit_list ) {
            auto const& unit = val_decl.decl_unit;

            // MEMO: do NOT check redefinition

            // TODO: make method to determine "type"

            // unit.kind -> val or ref
            // TODO: use unit.kind( default val )

            // TODO: evaluate type || type inference || type check
            if ( unit.init_unit.type ) { // is parameter variable type specified ?
                // in class, 'mutable' is DEFAULT!
                resolve_type(
                    unit.init_unit.type,
                    attribute::make( val_decl.quality, attribute::modifiability_kind::k_mutable ),
                    parent_env,
                    [&]( type_detail_ptr const& ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        auto attr = ty.attributes;

                        //
                        rill_dout << "class variable: " << unit.name->get_inner_symbol()->to_native_string() << std::endl;

                        if ( class_env->get_builtin_kind() == class_builtin_kind::k_void ) {
                            assert( false && "[error] void is not able to be instanced" );
                        }

                        //
                        if ( !class_env->is_default_copyable() ) {
                            parent_c_env->set_traits_flag( class_traits_kind::k_has_non_default_copyable_member, true );
                        }

                        //
                        {
                            auto const& padding
                                = parent_c_env->get_target_size()
                                % std::max( class_env->get_target_align(), std::size_t{1u} );
                            auto const& size = padding + class_env->get_target_size();

                            //
                            // set alignment
                            parent_c_env->set_target_size(
                                parent_c_env->get_target_size() + size
                                );

                            // set alignment
                            parent_c_env->set_target_align(
                                std::max(
                                    parent_c_env->get_target_align(),
                                    class_env->get_target_align()
                                    )
                                );
                        }

                        // completion
                        v_env->complete(
                            g_env_->make_type_id(
                                class_env,
                                attr
                                )
                            );
                    });

            } else {
                // type inferenced by result of evaluated [[default initializer expression]]

                // TODO: implement type inference
                assert( false );
            }
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::function_definition_statement, s, parent_env )
        {
            rill_dout
                << " != Semantic" << std::endl
                << "    function_definition_statement: " << std::endl
                << "     name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << "     Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = cast_to<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            rill_dout << "$" << std::endl;
            // guard double check
            if ( f_env->is_checked() ) return;
            f_env->change_progress_to_checked();
            rill_dout << "$ uncheckd" << std::endl;

            block_envs_.emplace( f_env );
            BOOST_SCOPE_EXIT_ALL(this) {
                block_envs_.pop();
            };

            //
            declare_function_parameters( f_env, s );

            // return type
            if ( s->return_type_ ) {
                // if return type was specified, decide type to it.
                resolve_type(
                    s->return_type_,
                    attribute::holder_kind::k_val,     // TODO: fix
                    f_env,
                    [&]( type_detail_ptr const& return_ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        rill_dout << "return type is >>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

                        f_env->decide_return_type( return_ty_d->type_id );
                    });
            }

            // scan all statements in this function body
            rill_dout << ">>>>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;
            dispatch( s->inner_, f_env );
            rill_dout << "<<<<" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            rill_dout << "returned: " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            // Return type
            solve_function_return_type_semantics( f_env );

            //
            f_env->complete( make_mangled_name( g_env_, f_env ), s->decl_attr_ );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::class_function_definition_statement, s, parent_env )
        {
            rill_dout
                << " != Semantic" << std::endl
                << "    CLASS function_definition_statement: " << std::endl
                << "     name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << "     Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = cast_to<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            rill_dout << "$" << std::endl;
            // guard double check
            if ( f_env->is_checked() ) return;
            f_env->change_progress_to_checked();
            rill_dout << "$ unchecked" << std::endl;

            block_envs_.emplace( f_env );
            BOOST_SCOPE_EXIT_ALL(this) {
                block_envs_.pop();
            };

            auto const& parent_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment>(
                    f_env->get_parent_class_env_id()
                    );
            assert( parent_c_env != nullptr );

            // TODO: fix
            bool const is_constructor
                = s->get_identifier()->get_inner_symbol()->to_native_string() == "ctor";

            //
            declare_function_parameters( f_env, s );

            if ( is_constructor ) {
                // constructor
                if ( s->return_type_ != nullptr ) {
                    semantic_error(
                        message_code::e_constructor_has_return_type,
                        s,
                        format( "Constructors can not have a return type" )
                        );
                }

                // force set, return type must be void
                auto const& void_class_env = get_primitive_class_env( "void" );
                auto ret_ty_id = g_env_->make_type_id(
                    void_class_env,
                    attribute::make_default_type_attributes()
                    );
                f_env->decide_return_type( ret_ty_id );

            } else {
                // normal function

                if ( s->initializers != boost::none ) {
                    semantic_error(
                        message_code::e_class_function_has_initializers,
                        s,
                        format( "Normal class functions can not have an initializer list" )
                        );
                }

                // return type
                if ( s->return_type_ ) {
                    // if return type was specified, decide type to it.
                    resolve_type(
                        s->return_type_,
                        attribute::holder_kind::k_val,     // TODO: fix
                        f_env,
                        [&]( type_detail_ptr const& return_ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            rill_dout << "return type is >>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

                            f_env->decide_return_type( return_ty_d->type_id );
                        });
                }
            }

            // initializer list
            if ( s->initializers ) {
                // list
                for( auto&& var_unit : s->initializers->initializers ) {
                    assert( var_unit.init_unit.initializer != nullptr );
                    assert( var_unit.init_unit.type == nullptr );

                    // initial value
                    auto const& iv_type_d
                        = dispatch( var_unit.init_unit.initializer, f_env );
                    assert( iv_type_d != nullptr );

                    // search class variable
                    auto const& d_ty
                        = solve_identifier( var_unit.name, parent_c_env, false );
                    if ( d_ty == nullptr ) {
                        semantic_error(
                            message_code::e_id_not_found,
                            var_unit.name,
                            format( "Identifier '%1%' was not declared." ) % var_unit.name->get_inner_symbol()->to_native_string()
                            );
                    }

                    //
                    auto const& element_env
                        = g_env_->get_related_env_by_ast_ptr( var_unit.name );
                    assert( element_env != nullptr );
                    if ( element_env->get_symbol_kind() != kind::type_value::e_variable ) {
                        assert( false );
                    }
                    auto const& v_env
                        = cast_to<variable_symbol_environment const>( element_env );
                    assert( v_env != nullptr );

                    // type check
                    RILL_PP_TIE( level, conv_function_env,
                                 try_type_conversion(
                                     d_ty->type_id,
                                     iv_type_d->type_id,
                                     parent_env
                                     )
                        );

                    switch( level ) {
                    case function_match_level::k_exact_match:
                        rill_dout << "Exact" << std::endl;
                        break;
                    case function_match_level::k_qualifier_conv_match:
                        rill_dout << "Qual" << std::endl;
                        break;
                    case function_match_level::k_implicit_conv_match:
                        rill_dout << "Implicit" << std::endl;
                        break;
                    case function_match_level::k_no_match:
                        rill_dout << "NoMatch" << std::endl;
                        assert( false && "[[error]] This value can not be assigned(type conversion)" );
                    }
                }
            }

            // scan all statements in this function body
            rill_dout << ">>>>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;
            dispatch( s->inner_, f_env );
            rill_dout << "<<<<" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            rill_dout << "returned: " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            // Return type
            solve_function_return_type_semantics( f_env );

            if ( !f_env->is_closed() ) {
                //
                s->inner_->statements_.emplace_back(
                    std::make_shared<ast::return_statement>( nullptr )
                    );
            }

            //
            f_env->complete( make_mangled_name( g_env_, f_env ), s->decl_attr_ );
        }


        RILL_VISITOR_OP( analyzer, ast::class_definition_statement, s, parent_env )
        {
            // TODO: dup check...
            // enverinment is already pre constructed by identifier_collector
            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_class );

            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment>( related_env );
            assert( c_env != nullptr );

            //
            complete_class( s, c_env );
        }


        RILL_VISITOR_OP( analyzer, ast::while_statement, s, parent_env )
        {
            auto const& scope_env = g_env_->allocate_env<scope_environment>( parent_env ); // MEMO:
            scope_env->link_with_ast( s );

            // TODO: type check
            dispatch( s->conditional_, scope_env );
            parent_env->mask_is_closed( scope_env->is_closed() );

            auto const& body_scope_env = g_env_->allocate_env<scope_environment>( scope_env );
            body_scope_env->link_with_ast( s->body_statement_ );

            dispatch( s->body_statement_, body_scope_env );
            parent_env->mask_is_closed( body_scope_env->is_closed() );
        }


        RILL_VISITOR_OP( analyzer, ast::if_statement, s, parent_env )
        {
            auto const& if_scope_env = g_env_->allocate_env<scope_environment>( parent_env ); // MEMO:
            if_scope_env->link_with_ast( s );

            // cond
            dispatch( s->conditional_, if_scope_env );  // TODO: type check
            parent_env->mask_is_closed( if_scope_env->is_closed() );

            // then
            auto const& if_then_env = g_env_->allocate_env<scope_environment>( if_scope_env );
            if_then_env->link_with_ast( s->then_statement_ );
            dispatch( s->then_statement_, if_then_env );
            bool const is_closed_0 = if_then_env->is_closed();

            // else( optional )
            bool is_closed_1 = false;
            if ( s->else_statement_ ) {
                auto const& if_else_env = g_env_->allocate_env<scope_environment>( if_scope_env );
                if_else_env->link_with_ast( s->else_statement_ );
                dispatch( s->else_statement_, if_else_env );
                is_closed_1 = if_else_env->is_closed();
            }

            parent_env->mask_is_closed( is_closed_0 && is_closed_1 );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::extern_function_declaration_statement, s, parent_env )
        {
            rill_dout
                << "= extern_function_definition_statement:" << std::endl
                << " Name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << " Args num -- " << s->get_parameter_list().size() << std::endl
                << " Parent env -- " << (const_environment_base_ptr)parent_env << std::endl;

            // enverinment is already pre constructed by identifier_collector
            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = cast_to<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->change_progress_to_checked();

            block_envs_.emplace( f_env );
            BOOST_SCOPE_EXIT_ALL(this) {
                block_envs_.pop();
            };

            // construct function environment in progress phase

            declare_function_parameters( f_env, s );

            //
            // Return type
            if ( s->return_type_ ) {
                resolve_type(
                    s->return_type_,
                    attribute::holder_kind::k_val,     // TODO: fix
                    parent_env,
                    [&]( type_detail_ptr const& return_ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        f_env->decide_return_type( return_ty_d->type_id );
                        f_env->complete(
                            make_mangled_name( g_env_, f_env ),
                            attribute::decl::k_extern | s->decl_attr_
                            );
                    });

            } else {
                // extern function MUST specifies RETURN TYPE. [[compilation error]]
                assert( false && "" );
            }


            //
            if ( f_env->has_attribute( attribute::decl::k_intrinsic ) ) {
                if ( auto&& id = action_holder_->is_registered( s->extern_symbol_name_ ) ) {
                    f_env->mark_as_intrinsic_function( *id );

                } else {
                    rill_dout << s->extern_symbol_name_ << std::endl;
                    assert( false && "[error] this intrinsic function is not registered" );
                }
            }
        }

        RILL_VISITOR_OP( analyzer, ast::extern_class_declaration_statement, s, parent_env )
        {
            // TODO: dup check...
            // enverinment is already pre constructed by identifier_collector
            auto const related_env = g_env_->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_class );

            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment>( related_env );
            assert( c_env != nullptr );

            complete_class( s, c_env );
        }

    } // namespace semantic_analysis
} // namespace rill
