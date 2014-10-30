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
#include <rill/utility/tie.hpp>


namespace rill
{
    namespace semantic_analysis
    {

        // Root Scope
        RILL_VISITOR_OP( analyzer, ast::statements, s, parent_env )
        {
            // build environment
            for( auto const& ss : s->statements_ )
                dispatch( ss, parent_env );
        }


        // Root Scope
        RILL_VISITOR_OP( analyzer, ast::block_statement, s, parent_env )
        {
            auto const& scope_env = parent_env->allocate_env<scope_environment>();
            scope_env->link_with_ast( s );

            dispatch( s->statements_, scope_env );
        }


        // statement
        RILL_VISITOR_OP( analyzer, ast::expression_statement, s, parent_env )
        {
            dispatch( s->expression_, parent_env );
        }

        //
        RILL_VISITOR_OP( analyzer, ast::return_statement, s, parent_env )
        {
            // Return Statement is valid only in Function Envirionment...
            auto const& a_env = parent_env->lookup_layer( kind::type_value::e_function );
            assert( a_env != nullptr ); // TODO: change to error_handler

            std::cout << "Return Statement [target: f_env] ------>> " << std::endl
                      << " type: " << debug_string( a_env->get_symbol_kind() ) << std::endl
                      << " env : " << a_env << std::endl;

            auto const& callee_f_env = cast_to<function_symbol_environment>( a_env );
            assert( callee_f_env != nullptr );

            auto const& return_type_detail
                = dispatch( s->expression_, parent_env );

            assert( !is_nontype_id( return_type_detail->type_id ) && "[[CE]] this object couldn't be returned" );

            callee_f_env->add_return_type_candidate( return_type_detail->type_id );
        }



        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::variable_declaration_statement, s, parent_env )
        {
            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env == nullptr && "[[ice]]" ); // ??

            auto const& val_decl = s->declaration_;
            // TODO: decl_unit will be unit_list
            // for( auto const& unit : val_decl.decl_unit_list ) {
            auto const& unit = val_decl.decl_unit;

            if ( auto const& v = parent_env->find_on_env( unit.name ) ) {
                assert( false && "[[error]] variable is already defined" );
            }

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

                        auto attr = ty.attributes;

                        // define variable
                        auto variable_env
                            = parent_env->construct(
                                kind::k_variable,
                                unit.name,
                                s,
                                class_env,
                                attr
                                );

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
                                std::cout << "Exact" << std::endl;
                                break;
                            case function_match_level::k_qualifier_conv_match:
                                std::cout << "Qual" << std::endl;
                                break;
                            case function_match_level::k_implicit_conv_match:
                                std::cout << "Implicit" << std::endl;
                                break;
                            case function_match_level::k_no_match:
                                std::cout << "NoMatch" << std::endl;
                                assert( false && "[[error]] This value can not be assigned(type conversion)" );
                            }
                        }
                    });

            } else {
                // type inferenced by result of evaluated "iv_type_id_and_env"
                assert( iv_type_d != nullptr );
                auto const& ty = root_env_->get_type_at( iv_type_d->type_id );
                auto const& c_env = std::static_pointer_cast<class_symbol_environment const>(
                    root_env_->get_env_strong_at( ty.class_env_id )
                    );

                //
                parent_env->construct(
                    kind::k_variable,
                    unit.name,
                    s,
                    c_env,
                    ty.attributes
                    );
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

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            // Forward referencable
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_variable );

            auto const& v_env = std::static_pointer_cast<variable_symbol_environment>( related_env );
            assert( v_env != nullptr );


            // guard double check
            if ( v_env->is_checked() )
                return;
            v_env->change_progress_to_checked();

            auto const& val_decl = s->declaration_;
            // TODO: decl_unit will be unit_list
            // for( auto const& unit : val_decl.decl_unit_list ) {
            auto const& unit = val_decl.decl_unit;


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
                        auto attr = ty.attributes;

                        //
                        std::cout << "class variable: " << unit.name->get_inner_symbol()->to_native_string() << std::endl;

                        // completion
                        v_env->complete(
                            v_env->make_type_id(
                                class_env,
                                attr
                                ),
                            unit.name->get_inner_symbol()->to_native_string()
                            );

                        //v_env->set_parent_class_env_id( class_env->get_id() );
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
            std::cout
                << " != Semantic" << std::endl
                << "    function_definition_statement: " << std::endl
                << "     name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << "     Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = cast_to<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            std::cout << "$" << std::endl;
            // guard double check
            if ( f_env->is_checked() ) return;
            f_env->change_progress_to_checked();
            std::cout << "$ uncheckd" << std::endl;

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    resolve_type(
                        e.decl_unit.init_unit.type,
                        e.quality,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;

                            // declare
                            f_env->parameter_variable_construct(
                                e.decl_unit.name,
                                class_env,
                                attr
                                );
                        });

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    assert( false );
                }
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
                        std::cout << "return type is >>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

                        f_env->decide_return_type( return_ty_d->type_id );
                    });
            }

            // scan all statements in this function body
            std::cout << ">>>>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;
            dispatch( s->inner_, f_env );
            std::cout << "<<<<" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            std::cout << "returned: " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            // Return type
            solve_function_return_type_semantics( f_env );

            //
            f_env->complete( make_mangled_name( f_env ) );
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::class_function_definition_statement, s, parent_env )
        {
            std::cout
                << " != Semantic" << std::endl
                << "    CLASS function_definition_statement: " << std::endl
                << "     name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << "     Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = cast_to<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            std::cout << "$" << std::endl;
            // guard double check
            if ( f_env->is_checked() ) return;
            f_env->change_progress_to_checked();
            std::cout << "$ unchecked" << std::endl;

            // declare "this" at first
            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    f_env->get_env_strong_at( f_env->get_parent_class_env_id() )
                    );
            assert( c_env != nullptr );

            // TODO: fix
            bool const is_constructor
                = s->get_identifier()->get_inner_symbol()->to_native_string() == "ctor";

            // TODO: see attribute of function and decide this type
            // currentry mutable, ref
            attribute::type_attributes const& this_object_attr
                = attribute::make_type_attributes(
                    attribute::holder_kind::k_ref,
                    [&]() {
                        if ( is_constructor ) {
                            return attribute::modifiability_kind::k_mutable;
                        } else {
                            // TODO: see member function modifier
                            return attribute::modifiability_kind::k_const;
                        }
                    }()
                    );

            // declare
            f_env->parameter_variable_construct(
                ast::make_identifier( "this" ),
                c_env,
                this_object_attr
                );

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    resolve_type(
                        e.decl_unit.init_unit.type,
                        e.quality,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;

                            // declare
                            f_env->parameter_variable_construct(
                                e.decl_unit.name,
                                class_env,
                                attr
                                );
                        });

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    assert( false );
                }
            }

            if ( is_constructor ) {
                // constructor
                assert( s->return_type_ == nullptr && "constructor can not have a return type" );

                auto const& void_class_env = get_primitive_class_env( "void" );
                auto ret_ty_id = root_env_->make_type_id( void_class_env, attribute::make_default_type_attributes() );

                f_env->decide_return_type( ret_ty_id );

            } else {
                // normal function
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
                            std::cout << "return type is >>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

                            f_env->decide_return_type( return_ty_d->type_id );
                        });
                }
            }

            // scan all statements in this function body
            std::cout << ">>>>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;
            dispatch( s->inner_, f_env );
            std::cout << "<<<<" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            std::cout << "returned: " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            // Return type
            solve_function_return_type_semantics( f_env );

            //
            f_env->complete( make_mangled_name( f_env ) );
        }





        RILL_VISITOR_OP( analyzer, ast::class_definition_statement, s, parent_env )
        {
            // TODO: dup check...
            // enverinment is already pre constructed by identifier_collector
            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_class );

            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment>( related_env );
            assert( c_env != nullptr );

            //
            complete_class( s, c_env );
        }







        RILL_VISITOR_OP( analyzer, ast::test_while_statement, s, parent_env )
        {
            auto const& scope_env = parent_env->allocate_env<scope_environment>();
            scope_env->link_with_ast( s );

            // TODO: type check
            dispatch( s->conditional_, scope_env );

/*
            auto const& body_scope_env = parent_env->allocate_env<scope_environment>( scope_env );
            body_scope_env->link_with_ast( s->body_statement_ );
*/
            dispatch( s->body_statement_, scope_env );
        }



        RILL_VISITOR_OP( analyzer, ast::test_if_statement, s, parent_env )
        {
            // if
            auto const& if_scope_env = parent_env->allocate_env<scope_environment>();
            if_scope_env->link_with_ast( s );
            dispatch( s->conditional_, if_scope_env );  // TODO: type check

            // then
            dispatch( s->then_statement_, if_scope_env );

            // else( optional )
            if ( s->else_statement_ ) {
                dispatch( *s->else_statement_, if_scope_env );
            }
        }



        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::extern_function_declaration_statement, s, parent_env )
        {
            std::cout
                << "= extern_function_definition_statement:" << std::endl
                << " Name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << " Args num -- " << s->get_parameter_list().size() << std::endl
                << " Parent env -- " << (const_environment_base_ptr)parent_env << std::endl;

            // enverinment is already pre constructed by identifier_collector
            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->change_progress_to_checked();

            // construct function environment in progress phase

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    resolve_type(
                        e.decl_unit.init_unit.type,
                        e.quality,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;

                            // declare
                            f_env->parameter_variable_construct(
                                /*TODO: add attributes, */
                                e.decl_unit.name,
                                class_env,
                                attr
                            );
                        });

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    assert( false );
                }
            }


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
                            make_mangled_name( f_env ),
                            function_symbol_environment::attr::e_extern
                            );
                    });

            } else {
                // extern function MUST specifies RETURN TYPE. [[compilation error]]
                assert( false && "" );
            }

            // TODO: add duplicate check
            //f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_base_ptr const)f_env << std::endl;
        }

    } // namespace semantic_analysis
} // namespace rill
