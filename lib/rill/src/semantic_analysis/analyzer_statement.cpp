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

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {

        // Root Scope
        RILL_TV_OP( analyzer, ast::statements, s, parent_env )
        {
            // build environment
            for( auto const& ss : s->statement_list_ )
                dispatch( ss, parent_env );
        }


        // Root Scope
        RILL_TV_OP( analyzer, ast::block_statement, s, parent_env )
        {
            auto const& scope_env = parent_env->allocate_env<scope_environment>( parent_env );
            scope_env->link_with_ast( s );

            dispatch( s->statements_, scope_env );
        }


        // statement
        // virtual void operator()( template_statement const& s, environment_base_ptr const& env ) const =0;

        RILL_TV_OP( analyzer, ast::expression_statement, s, parent_env )
        {
            dispatch( s->expression_, parent_env );
        }

        //
        RILL_TV_OP( analyzer, ast::return_statement, s, parent_env )
        {
            // Return Statement is valid only in Function Envirionment...
            auto const& a_env = parent_env->lookup_layer( kind::type_value::e_function );
            assert( a_env != nullptr ); // TODO: change to error_handler

            auto const type_id_and_env
                = dispatch( s->expression_, parent_env );

            assert( type_id_and_env.type_id != type_id_special && "[[CE]] this object couldn't be returned" );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( a_env );
            f_env->add_return_type_candidate( type_id_and_env.type_id );
        }


        // TODO: change to ctfe_expression
        RILL_TV_OP( analyzer, ast::jit_statement, s, parent_env )
        {
            // Return Statement is valid only in Function Envirionment...
            auto const& a_env = parent_env->lookup_layer( kind::type_value::e_function );
            assert( a_env != nullptr ); // TODO: change to error_handler

            auto const type_id_and_env
                = dispatch( s->expression_, parent_env );

            assert( type_id_and_env.type_id != type_id_special && "[[CE]] this object couldn't be returned" );


//            run_on_compile_time( parent_env, s->expression_ );


            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( a_env );
            f_env->add_return_type_candidate( type_id_and_env.type_id );
        }





        //
        //
        //
        RILL_TV_OP( analyzer, ast::variable_declaration_statement, s, parent_env )
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
                : []() ->  type_id_with_env
                {
                    //assert( false && "[[]] Currently, uninitialized value was not supported..." );
                    return {
                        type_id_undefined,
                        nullptr
                    };
                }();

            // TODO: make method to determine "type"

            // unit.kind -> val or ref
            // TODO: use unit.kind( default val )

            // TODO: evaluate type || type inference || type check
            //       default( int )

            if ( unit.init_unit.type ) { // is parameter variable type specified ?
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( parent_env, unit.init_unit.type );

                // in declaration unit, can not specify "quality" by type_expression
                assert( type_value.attributes.quality == boost::none );

                if ( auto const class_env = std::static_pointer_cast<class_symbol_environment>( lookup_with_instanciation( parent_env, type_value.identifiers ) ) ) {
                    assert( class_env != nullptr );
                    assert( class_env->get_symbol_kind() == kind::type_value::e_class );

                    if ( class_env->is_incomplete() ) {
                        //
                        auto const& class_node = class_env->get_related_ast();
                        assert( class_node != nullptr );

                        dispatch( class_node, class_env->get_parent_env() );
                        assert( class_env->is_complete() );
                    }

                    auto attr = determine_type_attributes( type_value.attributes );
                    attr <<= val_decl.quality;

                    // TODO: type check with "iv_type_id_and_env"

                    // declare
                    auto variable_env
                        = parent_env->construct(
                            kind::k_variable,
                            unit.name,
                            s,
                            std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
                            attr
                            );

                    //
                    // variable_env->connect_from_ast( s );

                } else {
                    // type was not found, !! compilation error !!
                    assert( false && "type was not found" );
                }

            } else {
                // type inferenced by result of evaluated "iv_type_id_and_env"

                // TODO: implement type inference
                assert( false && "[[ICE]] not implemented");
            }
        }





        //
        //
        //
        RILL_TV_OP( analyzer, ast::class_variable_declaration_statement, s, parent_env )
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
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( parent_env, unit.init_unit.type );

                // in declaration unit, can not specify "quality" by type_expression
                assert( type_value.attributes.quality == boost::none );

                if ( auto const class_env = lookup_with_instanciation( parent_env, type_value.identifiers ) ) {
                    assert( class_env != nullptr );
                    assert( class_env->get_symbol_kind() == kind::type_value::e_class );

                    auto attr = determine_type_attributes( type_value.attributes );
                    attr <<= val_decl.quality;

                    //
                    std::cout << "class variable: " << unit.name->get_inner_symbol()->to_native_string() << std::endl;

                    // completion
                    v_env->complete(
                        v_env->make_type_id(
                            std::static_pointer_cast<class_symbol_environment const>( class_env ),
                            attr
                            ),
                        unit.name->get_inner_symbol()->to_native_string()
                        );

                    //v_env->set_parent_class_env_id( class_env->get_id() );

                } else {
                    // type was not found, !! compilation error !!
                    assert( false && "type was not found" );
                }

            } else {
                // type inferenced by result of evaluated [[default initializer expression]]

                // TODO: implement type inference
                assert( false );
            }


            
        }








        //
        //
        //
        RILL_TV_OP( analyzer, ast::function_definition_statement, s, parent_env )
        {
            std::cout
                << "function_definition_statement: ast_ptr -> "
                << (environment_base_ptr const&)parent_env << std::endl
                << "name -- " << s->get_identifier()->get_inner_symbol()->to_native_string() << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->change_progress_to_checked();



            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    // evaluate constant expresison as type
                    auto const& type_value = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    // in declaration unit, can not specify "quality" by type_expression
                    assert( type_value.attributes.quality == boost::none );


                    if ( auto const class_env = lookup_with_instanciation( f_env, type_value.identifiers ) ) {
                        assert( class_env != nullptr );
                        assert( class_env->get_symbol_kind() == kind::type_value::e_class );

                        auto attr = determine_type_attributes( type_value.attributes );
                        attr <<= e.quality;

                        // declare
                        f_env->parameter_variable_construct(
                            e.decl_unit.name,
                            std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
                            attr
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
            dispatch( s->inner_, f_env );


            // ?: TODO: use block expression


            // Return type
            if ( s->return_type_ ) {
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( f_env, *s->return_type_ );

                if ( auto const return_class_env = lookup_with_instanciation( f_env, type_value.identifiers ) ) {
                    assert( return_class_env != nullptr );
                    assert( return_class_env->get_symbol_kind() == kind::type_value::e_class );

                    // TODO: check return statement types...
                    // f_env->get_return_type_candidates()

                    //
                    auto const& return_type_id = f_env->make_type_id(
                        return_class_env,
                        determine_type_attributes( type_value.attributes )
                        );
                    f_env->complete( return_type_id, s->get_identifier()->get_inner_symbol()->to_native_string() );

                } else {
                    // type was not found, !! compilation error !!
                    assert( false );
                }

            } else {
                // TODO: implement return type inference
                assert( false && "function return type inference was not supported yet" );
            }

            //
            f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_base_ptr const)f_env << std::endl;
        }





        //
        //
        //
        RILL_TV_OP( analyzer, ast::class_function_definition_statement, s, parent_env )
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

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    // evaluate constant expresison as type
                    auto const& type_value = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    // in declaration unit, can not specify "quality" by type_expression
                    assert( type_value.attributes.quality == boost::none );


                    if ( auto const class_env = lookup_with_instanciation( f_env, type_value.identifiers ) ) {
                        assert( class_env != nullptr );
                        assert( class_env->get_symbol_kind() == kind::type_value::e_class );

                        auto attr = determine_type_attributes( type_value.attributes );
                        attr <<= e.quality;

                        // declare
                        f_env->parameter_variable_construct(
                            e.decl_unit.name,
                            std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
                            attr
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
            dispatch( s->inner_, f_env );

            // ?: TODO: use block expression


            // Return type
            if ( s->return_type_ ) {
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( f_env, *s->return_type_ );

                if ( auto const return_class_env = lookup_with_instanciation( f_env, type_value.identifiers ) ) {
                    assert( return_class_env != nullptr );
                    assert( return_class_env->get_symbol_kind() == kind::type_value::e_class );

                    // TODO: check return statement types...
                    // f_env->get_return_type_candidates()

                    //
                    auto const& return_type_id = f_env->make_type_id(
                        return_class_env,
                        determine_type_attributes( type_value.attributes )
                        );
                    f_env->complete( return_type_id, s->get_identifier()->get_inner_symbol()->to_native_string() );

                } else {
                    // type was not found, !! compilation error !!
                    assert( false );
                }

            } else {
                // TODO: implement return type inference
                assert( false && "function return type inference was not supported yet" );
            }

            //
            f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_base_ptr const)f_env << std::endl;
        }





        RILL_TV_OP( analyzer, ast::class_definition_statement, s, parent_env )
        {
            // TODO: dup check...
            // enverinment is already pre constructed by identifier_collector
            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_class );

            auto const& c_env
                = std::static_pointer_cast<class_symbol_environment>( related_env );
            assert( c_env != nullptr );

            // guard double check
            if ( c_env->is_checked() ) {
                std::cout << "Already, checked" << std::endl;
                return;
            }
            c_env->change_progress_to_checked();

            dispatch( s->inner_, c_env );

            c_env->complete( s->get_identifier()->get_inner_symbol()->to_native_string() );
        }



        RILL_TV_OP( analyzer, ast::test_while_statement, s, parent_env )
        {
            auto const& scope_env = parent_env->allocate_env<scope_environment>( parent_env );
            scope_env->link_with_ast( s );

            // TODO: type check
            dispatch( s->conditional_, scope_env );

/*
            auto const& body_scope_env = parent_env->allocate_env<scope_environment>( scope_env );
            body_scope_env->link_with_ast( s->body_statement_ );
*/
            dispatch( s->body_statement_, scope_env );
        }



        RILL_TV_OP( analyzer, ast::test_if_statement, s, parent_env )
        {
            // if 
            auto const& if_scope_env = parent_env->allocate_env<scope_environment>( parent_env );
            if_scope_env->link_with_ast( s );
            dispatch( s->conditional_, if_scope_env );  // TODO: type check

            // then
/*
            auto const& then_scope_env = parent_env->allocate_env<scope_environment>( if_scope_env );
            then_scope_env->link_with_ast( s->then_statement_ );            
*/
            dispatch( s->then_statement_, if_scope_env/*then_scope_env*/ );

            // else( optional )
            if ( s->else_statement_ ) {
/*
                auto const& else_scope_env = parent_env->allocate_env<scope_environment>( if_scope_env );
                else_scope_env->link_with_ast( *s->else_statement_ );            
*/
                dispatch( *s->else_statement_, if_scope_env/*then_scope_env*/ );
            }
        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::extern_function_declaration_statement, s, parent_env )
        {
            std::cout
                << "function_definition_statement: ast_ptr -> "
                << (environment_base_ptr const&)parent_env << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

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
                    // evaluate constant expresison as type
                    auto const& type = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    if ( auto const type_env = lookup_with_instanciation( f_env, type.identifiers ) ) {
                        assert( type_env != nullptr );
                        assert( type_env->get_symbol_kind() == kind::type_value::e_class );

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


            //
            // Return type
            if ( s->return_type_ ) {
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( f_env, s->return_type_ );

                if ( auto const return_class_env = lookup_with_instanciation( f_env, type_value.identifiers ) ) {
                    assert( return_class_env != nullptr );
                    assert( return_class_env->get_symbol_kind() == kind::type_value::e_class );

                    //
                    auto const& return_type_id = f_env->make_type_id(
                        return_class_env,
                        determine_type_attributes( type_value.attributes )
                        );
                    f_env->complete( return_type_id, s->get_identifier()->get_inner_symbol()->to_native_string(), function_symbol_environment::attr::e_extern );

                } else {
                    // type was not found, !! compilation error !!
                    assert( false );
                }

            } else {
                // extern function MUST specifies RETURN TYPE. [[compilation error]]
                assert( false && "" );
            }

            // TODO: add duplicate check
            f_env->get_parameter_wrapper_env()->add_overload( f_env );

            std::cout << (environment_base_ptr const)f_env << std::endl;
        }

    } // namespace semantic_analysis
} // namespace rill
