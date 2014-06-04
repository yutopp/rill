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


namespace rill
{
    namespace semantic_analysis
    {

        // Root Scope
        RILL_VISITOR_OP( analyzer, ast::statements, s, parent_env )
        {
            // build environment
            for( auto const& ss : s->statement_list_ )
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

            auto const t_detail
                = dispatch( s->expression_, parent_env );

            assert( !is_nontype_id( t_detail->type_id ) && "[[CE]] this object couldn't be returned" );

            auto const& callee_f_env = std::static_pointer_cast<function_symbol_environment>( a_env );
            callee_f_env->add_return_type_candidate( t_detail->type_id );
        }


        // TODO: change to ctfe_expression
        RILL_VISITOR_OP( analyzer, ast::jit_statement, s, parent_env )
        {
            // Return Statement is valid only in Function Envirionment...
            auto const& a_env = parent_env->lookup_layer( kind::type_value::e_function );
            assert( a_env != nullptr ); // TODO: change to error_handler

            auto const t_detail
                = dispatch( s->expression_, parent_env );

            assert( !is_nontype_id( t_detail->type_id ) && "[[CE]] this object couldn't be returned" );


            std::cout << "!!! jit eval" << std::endl;
            ctfe_engine_->execute_as_raw_storage( s->expression_, parent_env );
            std::cout << "~~~~~~~~~~~~" << std::endl;
//            run_on_compile_time( parent_env, s->expression_ );


            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( a_env );
            f_env->add_return_type_candidate( t_detail->type_id );
        }





        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::variable_declaration_statement, s, parent_env )
        {
            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );

            if ( related_env != nullptr ) {
                assert( false && "[ice] duplicate..." );
            }

            auto const& val_decl = s->declaration_;
            // TODO: decl_unit will be unit_list
            // for( auto const& unit : val_decl.decl_unit_list ) {
            auto const& unit = val_decl.decl_unit;

            // initial value
            auto const& iv_type_id_and_env
                = unit.init_unit.initializer
                ? dispatch( unit.init_unit.initializer, parent_env )
                : [this]() -> type_detail_ptr
                {
                    //assert( false && "[[]] Currently, uninitialized value was not supported..." );
                    return type_detail_pool_->construct(
                        type_id_undefined,
                        nullptr
                    );
                }();

            // TODO: make method to determine "type"

            // unit.kind -> val or ref
            // TODO: use unit.kind( default val )

            // TODO: evaluate type || type inference || type check
            //       default( int )

            if ( unit.init_unit.type ) { // is parameter variable type specified ?
                solve_type(
                    this,
                    unit.init_unit.type,
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
                        attr <<= val_decl.quality;

                        std::cout << "Attr:! " << std::endl;
                        switch( attr.quality ) {
                        case attribute::quality_kind::k_suggest:
                            std::cout << "k_suggest" << std::endl;
                            break;

                        case attribute::quality_kind::k_val:
                            std::cout << "k_val" << std::endl;
                            break;

                        case attribute::quality_kind::k_ref:
                            std::cout << "k_ref" << std::endl;
                            break;
                        }

                        switch( attr.modifiability ) {
                        case attribute::modifiability_kind::k_mutable:
                            std::cout << "k_mutable" << std::endl;
                            break;

                        case attribute::modifiability_kind::k_const:
                            std::cout << "k_const" << std::endl;
                            break;

                        case attribute::modifiability_kind::k_immutable:
                            std::cout << "k_immutable" << std::endl;
                            break;
                        }



                        // TODO: type check with "iv_type_id_and_env"

                        // definition...
                        auto variable_env
                            = parent_env->construct(
                                kind::k_variable,
                                unit.name,
                                s,
                                class_env,
                                attr
                                );
                    });

            } else {
                // type inferenced by result of evaluated "iv_type_id_and_env"

                // TODO: implement type inference
                assert( false && "[[ICE]] not implemented");
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
                solve_type(
                    this,
                    unit.init_unit.type,
                    parent_env,
                    [&]( type_detail_ptr const& ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        auto attr = ty.attributes;
                        attr <<= val_decl.quality;

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
                    solve_type(
                        this,
                        e.decl_unit.init_unit.type,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;
                            attr <<= e.quality;

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

            // scan all statements in this function body
            std::cout << ">>>>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;
            dispatch( s->inner_, f_env );
            std::cout << "<<<<" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            // ?: TODO: use block expression

            std::cout << "returned: " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;

            // Return type
            if ( s->return_type_ ) {

                solve_type(
                    this,
                    *s->return_type_,
                    parent_env,
                    [&]( type_detail_ptr const& return_ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        // TODO: check return statement types...
                        // f_env->get_return_type_candidates()


                        std::cout << "<<>>" << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl;



                        f_env->complete(
                            return_ty_d->type_id,
                            make_mangled_name( f_env )
                            );
                    });

            } else {
                // TODO: implement return type inference
                assert( false && "function return type inference was not supported yet" );
            }

            //
            //f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_base_ptr const)f_env << std::endl;
        }


        //
        //
        //
        RILL_VISITOR_OP( analyzer, ast::class_function_definition_statement, s, parent_env )
        {
            std::cout
                << "function_definition_statement: ast_ptr -> "
                << (environment_base_ptr const&)parent_env << std::endl
                << "name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->change_progress_to_checked();


            // declare "this" at first
            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    f_env->get_env_strong_at( f_env->get_parent_class_env_id() )
                    );
            assert( c_env != nullptr );

            // TODO: see attribute of function and decide this type
            // currentry immutable, ref
            attribute::type_attributes const& this_object_attr
                = attribute::make_type_attributes(
                    attribute::quality_kind::k_ref,
                    attribute::modifiability_kind::k_immutable
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

                if ( e.decl_unit.init_unit.type ) { // is parameter variable type specified ?
                    solve_type(
                        this,
                        e.decl_unit.init_unit.type,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;
                            attr <<= e.quality;

                            // declare
                            f_env->parameter_variable_construct(
                                e.decl_unit.name,
                                class_env,
                                attr
                                );
                        } );

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    assert( false );
                }
            }

            // scan all statements in this function body
            dispatch( s->inner_, f_env );

            // ?: TODO: use block expression


            // Return type
            if ( s->return_type_ ) {
                solve_type(
                    this,
                    *s->return_type_,
                    parent_env,
                    [&]( type_detail_ptr const& return_ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        // TODO: check return statement types...
                        // f_env->get_return_type_candidates()

                        f_env->complete(
                            return_ty_d->type_id,
                            make_mangled_name( f_env )
                            );
                    });

            } else {
                // TODO: implement return type inference
                assert( false && "function return type inference was not supported yet" );
            }

            //
            //f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_base_ptr const)f_env << std::endl;
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



        auto analyzer::complete_class(
            ast::class_definition_statement_ptr const& s,
            class_symbol_environment_ptr const& c_env,
            type_detail::template_arg_pointer const& template_args
            )
            -> bool
        {
            // guard double check
            if ( c_env->is_checked() ) {
                std::cout << "Already, checked" << std::endl;
                // assert( false );
                return false;
            }
            c_env->change_progress_to_checked();


            //
            auto const& base_name
                = s->get_identifier()->get_inner_symbol()->to_native_string();
            auto const& qualified_name
                = s->get_identifier()->get_inner_symbol()->to_native_string();

            if ( s->inner_ != nullptr ) {
                // analyse class body
                dispatch( s->inner_, c_env );

                // complete class data
                c_env->complete(
                    base_name,
                    qualified_name,
                    class_attribute::structed
                    );

            } else {
                std::cout << "builtin class!" << std::endl;

                // complete class data
                c_env->complete(
                    base_name,
                    qualified_name,
                    class_attribute::none
                    );

                // TODO: change...;(;(;(
                if ( s->get_identifier()->get_inner_symbol()->to_native_string() == "array" ) {
                    // set special flag as Array
                    // array template args are
                    // [0]: type
                    // [1]: number of elements
                    assert( template_args->at( 0 ).is_type() );

                    auto const& array_element_ty_detail
                        = static_cast<type_detail_ptr>( template_args->at( 0 ).element );

                    llvm::ConstantInt const* const llvm_array_element_num
                        = static_cast<llvm::ConstantInt const* const>( template_args->at( 1 ).element );

                    std::size_t const array_element_num
                        = type_id_t( llvm_array_element_num->getZExtValue() );

                    c_env->make_as_array(
                        array_element_ty_detail->type_id,
                        array_element_num
                        );
                }
            }

            return true;
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

            dispatch( s->then_statement_, if_scope_env/*then_scope_env*/ );

            // else( optional )
            if ( s->else_statement_ ) {

                dispatch( *s->else_statement_, if_scope_env/*then_scope_env*/ );
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
                    solve_type(
                        this,
                        e.decl_unit.init_unit.type,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;
                            attr <<= e.quality;

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
                solve_type(
                    this,
                    s->return_type_,
                    parent_env,
                    [&]( type_detail_ptr const& return_ty_d,
                         type const& ty,
                         class_symbol_environment_ptr const& class_env
                        ) {
                        f_env->complete(
                            return_ty_d->type_id,
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
