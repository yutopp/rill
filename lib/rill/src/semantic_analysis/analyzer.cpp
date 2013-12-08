//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/environment/environment.hpp>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        static inline auto determine_type_attributes(
            attribute::type_attributes_optional const& attr = attribute::type_attributes_optional()
            )
            -> attribute::type_attributes
        {
            return attribute::make_type_attributes(
                attr.quality
                ? *attr.quality
                : attribute::quality_kind::k_val,                   // default value
                attr.modifiability
                ? *attr.modifiability
                : attribute::modifiability_kind::k_immutable        // defaut value
                );
        }


        template<typename IdetifierPtr, typename EnvPtr, typename ResultCallbackT>
        static auto overload_solver(
            type_id_list_t const& arg_type_ids,
            std::shared_ptr<has_parameter_environment<function_symbol_environment>> const& generic_function_env,
            IdetifierPtr const& identifier,
            EnvPtr const& env,
            ResultCallbackT const& f
            )
            -> function_symbol_environment_ptr
        {
            //
            //
            type_id_list_t holder( arg_type_ids.size() );
            std::vector<function_symbol_environment_ptr> f_candidate_envs;


            // DEBUG
            std::cout << "...resolving function : " << identifier->get_inner_symbol()->to_native_string() << std::endl;


            // TODO: fix over load solver
            // TODO: check variadic parameter...
            // TODO: count conversion times
            for( auto const& f_env : generic_function_env->get_overloads() ) {
                // DEBUG
                std::cout << "..." << f_env << std::endl;


                auto const& f_env_parameter_type_ids = f_env->get_parameter_type_ids();

                // argument size is different
                if ( f_env_parameter_type_ids.size() != arg_type_ids.size() )
                    continue;

                // has no argument
                if ( f_env_parameter_type_ids.size() == 0 ) {
                    // TODO: check context...(Ex. pure)
                    std::cout << "~~~0~~~~" << std::endl;
                    f_candidate_envs.push_back( f_env );
                    continue;
                }

                //
                bool succeed = true;
                for( int i=0; i<arg_type_ids.size(); ++i ) {
                    if ( f_env_parameter_type_ids[i] == arg_type_ids[i] ) {
                        // has same type!
                        holder[i] = arg_type_ids[i];

                    } else {
                        // try to type conversion
                        auto const& f_env_arg_type = f_env->get_type_at( f_env_parameter_type_ids[i] );
                        auto self_arg_type = env->get_type_at( arg_type_ids[i] );


                        // 1. try to attribute check and conversion

                        // 1.1 check quarity comversion
                        if ( f_env_arg_type.attributes.quality != self_arg_type.attributes.quality ) {
                            switch( f_env_arg_type.attributes.quality )
                            {
                            case attribute::quality_kind::k_ref:

                                switch( self_arg_type.attributes.quality )
                                {
                                case attribute::quality_kind::k_val:
                                    // val -> ref conversion

                                    if ( f_env_arg_type.attributes.modifiability != self_arg_type.attributes.modifiability ) {

                                        // check modifiability
                                        switch( f_env_arg_type.attributes.modifiability )
                                        {
                                        case attribute::modifiability_kind::k_mutable:

                                            switch( self_arg_type.attributes.modifiability )
                                            {
                                            case attribute::modifiability_kind::k_mutable:
                                                // mutable -> mutable : valid
                                                break;

                                            case attribute::modifiability_kind::k_const:
                                                // mutable -> const : valid
                                                break;

                                            case attribute::modifiability_kind::k_immutable:
                                                // mutable -> immutable : INVARID
                                                // TODO: check flag
                                                assert( false );
                                                break;

                                            default:
                                                assert( false && "[ice]" );
                                                break;
                                            }


                                            break;

                                        case attribute::modifiability_kind::k_const:
                                            // TODO: implement
                                            assert( false );
                                            break;

                                        case attribute::modifiability_kind::k_immutable:
                                            // TODO: implement
                                            assert( false );
                                            break;

                                        default:
                                            assert( false && "[ice]" );
                                            break;
                                        }


                                        // copy modifiablity
                                        self_arg_type.attributes <<= f_env_arg_type.attributes.modifiability;
                                    }

                                    break;

                                default:
                                    // TODO: implement
                                    assert( false && "[ice]" );
                                    break;
                                }





                                break;

                            case attribute::quality_kind::k_val:
                                // All type -> val is convertible at the moment
                                self_arg_type = f_env_arg_type; //attribute::quality_kind:k_val;

                                break;

                            default:
                                // TODO: implement
                                assert( false && "[ice]" );
                                break;
                            }
                        }
                        if ( !succeed )
                            break;   // change overload resolution target



                        // TODO: remove this
                        self_arg_type.attributes <<= f_env_arg_type.attributes.modifiability;




                        // 2. class type conversion
                        if ( f_env_arg_type.class_env_id != self_arg_type.class_env_id ) {
                            // TODO: implement
                            assert( false && "[ice]" );
                        }
                        if ( !succeed )
                            break;   // change overload resolution target

                        // rewrite
                        holder[i] = env->make_type_id( f_env_arg_type.class_env_id, f_env_arg_type.attributes );
                    }
                } // for
                if ( succeed ) {
                    f_candidate_envs.push_back( f_env );
                }
            }


            return f( f_candidate_envs );
        }


        template<typename IdetifierPtr, typename EnvPtr>
        static inline auto overload_solver(
            type_id_list_t const& arg_type_ids ,
            std::shared_ptr<has_parameter_environment<function_symbol_environment>> const& generic_function_env,
            IdetifierPtr const& identifier,
            EnvPtr const& env
            )
            -> function_symbol_environment_ptr
        {
            return overload_solver(
                arg_type_ids,
                generic_function_env,
                identifier,
                env,
                []( std::vector<function_symbol_environment_ptr> const& f_candidate_envs ) {

                    size_t selected = 0;
                    if ( f_candidate_envs.size() == 0 ) {
                        // TODO: to search other namespaces...
                        assert( false && "Overload failed... [not implemented]");
                    } else if ( f_candidate_envs.size() > 1 ) {
                        // TODO: check comversion times...
                        assert( false && "duplecate?... [not implemented]");
                    }

                    return f_candidate_envs[selected];
                }
                );
        }

        template<typename IdetifierPtr, typename EnvPtr>
        static inline auto overload_solver_allow_no_entry(
            type_id_list_t const& arg_type_ids ,
            std::shared_ptr<has_parameter_environment<function_symbol_environment>> const& generic_function_env,
            IdetifierPtr const& identifier,
            EnvPtr const& env
            )
            -> function_symbol_environment_ptr
        {
            return overload_solver(
                arg_type_ids,
                generic_function_env,
                identifier,
                env,
                []( std::vector<function_symbol_environment_ptr> const& f_candidate_envs )-> function_symbol_environment_ptr{

                    size_t selected = 0;
                    if ( f_candidate_envs.size() == 0 ) {
                        return nullptr;
                    } else if ( f_candidate_envs.size() > 1 ) {
                        // TODO: check comversion times...
                        assert( false && "duplecate?... [not implemented]");
                    }

                    return f_candidate_envs[selected];
                }
                );
        }



        // Root Scope
        RILL_TV_OP( analyzer, ast::root, r, parent_env )
        {
            // collect all type identifiers under this scope
            //collect_type_identifier( env, r.statements_ );

            // collect all identifiers(except types) under this scope
            collect_identifier( parent_env, r );

            // build environment
            for( auto const& node : r->statements_ )
                dispatch( node, parent_env );
        }


        // Root Scope
        RILL_TV_OP( analyzer, ast::block_statement, s, parent_env )
        {
            for( auto const& node : s->statements_ )
                dispatch( node, parent_env );
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

            auto const type_id = dispatch( s->expression_, parent_env );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( a_env );
            f_env->add_return_type_candidate( type_id );
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

                if ( auto const class_env = lookup_with_instanciation( parent_env, type_value.identifier ) ) {
                    assert( class_env != nullptr );
                    assert( class_env->get_symbol_kind() == kind::type_value::e_class );

                    auto attr = determine_type_attributes( type_value.attributes );
                    attr <<= val_decl.quality;

                    // declare
                    auto variable_env
                        = parent_env->construct(
                            kind::k_variable,
                            unit.name,
                            nullptr,
                            std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
                            attr
                            );

                    //
                    variable_env->connect_from_ast( s );

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
        RILL_TV_OP( analyzer, ast::class_variable_declaration_statement, s, parent_env )
        {
            assert( parent_env->get_symbol_kind() == kind::type_value::e_class );

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            // Forward referencable
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_variable );

            auto const& v_env = std::static_pointer_cast<variable_symbol_environment>( related_env );
            assert( v_env != nullptr );


/*
// guard double check
if ( f_env->is_checked() )
return;
f_env->check();
*/
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

                if ( auto const class_env = lookup_with_instanciation( parent_env, type_value.identifier ) ) {
                    assert( class_env != nullptr );
                    assert( class_env->get_symbol_kind() == kind::type_value::e_class );

                    auto attr = determine_type_attributes( type_value.attributes );
                    attr <<= val_decl.quality;

                    // declare
                    auto variable_env
                        = parent_env->construct(
                            kind::k_variable,
                            unit.name,
                            nullptr,
                            std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
                            attr
                            );

                    //
                    variable_env->connect_from_ast( s );

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
                << "name -- " << s->get_identifier()->last()->get_inner_symbol()->to_native_string() << std::endl
                << "Args num -- " << s->get_parameter_list().size() << std::endl;

            auto const related_env = parent_env->get_related_env_by_ast_ptr( s );
            assert( related_env != nullptr );
            assert( related_env->get_symbol_kind() == kind::type_value::e_function );

            auto const& f_env = std::static_pointer_cast<function_symbol_environment>( related_env );
            assert( f_env != nullptr );

            // guard double check
            if ( f_env->is_checked() )
                return;
            f_env->check();



            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    // evaluate constant expresison as type
                    auto const& type_value = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    // in declaration unit, can not specify "quality" by type_expression
                    assert( type_value.attributes.quality == boost::none );


                    if ( auto const class_env = lookup_with_instanciation( f_env, type_value.identifier ) ) {
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
            dispatch( s->block_, f_env );


            // ?: TODO: use block expression


            // Return type
            if ( s->return_type_ ) {
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( f_env, *s->return_type_ );

                if ( auto const return_class_env = lookup_with_instanciation( f_env, type_value.identifier ) ) {
                    assert( return_class_env != nullptr );
                    assert( return_class_env->get_symbol_kind() == kind::type_value::e_class );

                    // TODO: check return statement types...
                    // f_env->get_return_type_candidates()

                    //
                    auto const& return_type_id = f_env->make_type_id(
                        return_class_env,
                        determine_type_attributes( type_value.attributes )
                        );
                    f_env->complete( return_type_id, s->get_identifier()->last()->get_inner_symbol()->to_native_string() );

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
            f_env->check();


            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    // evaluate constant expresison as type
                    auto const& type_value = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    // in declaration unit, can not specify "quality" by type_expression
                    assert( type_value.attributes.quality == boost::none );


                    if ( auto const class_env = lookup_with_instanciation( f_env, type_value.identifier ) ) {
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
            dispatch( s->block_, f_env );

            // ?: TODO: use block expression


            // Return type
            if ( s->return_type_ ) {
                // evaluate constant expresison as type
                auto const& type_value = interpreter::evaluate_as_type( f_env, *s->return_type_ );

                if ( auto const return_class_env = lookup_with_instanciation( f_env, type_value.identifier ) ) {
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

            auto const& c_env = std::static_pointer_cast<class_symbol_environment>( related_env );
            assert( c_env != nullptr );

/*
            // guard double check
            if ( c_env->is_checked() )
                return;
            c_env->check();
*/

            dispatch( s->block_, c_env );
        }



        RILL_TV_OP( analyzer, ast::test_while_statement, s, parent_env )
        {
            auto const& scope_env = parent_env->allocate_env<scope_environment>( parent_env );
            scope_env->link_with_ast( s );

            // TODO: type check
            dispatch( s->conditional_, scope_env );

            auto const& body_scope_env = parent_env->allocate_env<scope_environment>( scope_env );
            body_scope_env->link_with_ast( s->body_statement_ ); 
            dispatch( s->body_statement_, scope_env );
        }



        RILL_TV_OP( analyzer, ast::test_if_statement, s, parent_env )
        {
            // if 
            auto const& if_scope_env = parent_env->allocate_env<scope_environment>( parent_env );
            if_scope_env->link_with_ast( s );
            dispatch( s->conditional_, if_scope_env );  // TODO: type check

            // then
            auto const& then_scope_env = parent_env->allocate_env<scope_environment>( if_scope_env );
            then_scope_env->link_with_ast( s->then_statement_ );            
            dispatch( s->then_statement_, then_scope_env );

            // else( optional )
            if ( s->else_statement_ ) {
                auto const& else_scope_env = parent_env->allocate_env<scope_environment>( if_scope_env );
                else_scope_env->link_with_ast( *s->else_statement_ );            
                dispatch( *s->else_statement_, else_scope_env );
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
            f_env->check();

            // construct function environment in progress phase

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                    // evaluate constant expresison as type
                    auto const& type = interpreter::evaluate_as_type( f_env, e.decl_unit.init_unit.type );

                    if ( auto const type_env = lookup_with_instanciation( f_env, type.identifier ) ) {
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

                if ( auto const return_class_env = lookup_with_instanciation( f_env, type_value.identifier ) ) {
                    assert( return_class_env != nullptr );
                    assert( return_class_env->get_symbol_kind() == kind::type_value::e_class );

                    //
                    auto const& return_type_id = f_env->make_type_id(
                        return_class_env,
                        determine_type_attributes( type_value.attributes )
                        );
                    f_env->complete( return_type_id, s->get_identifier()->last()->get_inner_symbol()->to_native_string(), function_symbol_environment::attr::e_extern );

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




        //
        // expressions
        //


        //
        RILL_TV_OP( analyzer, ast::binary_operator_expression, e, env )
        {
            // check type environment
            auto const& lhs_type_id = dispatch( e->lhs_, env );
            auto const& rhs_type_id = dispatch( e->rhs_, env );

            assert( lhs_type_id != type_id_undefined && rhs_type_id != type_id_undefined && "[ice]" );

            // make argument types id list
            type_id_list_t const argument_type_ids = { lhs_type_id, rhs_type_id };


            // DEBUG
            std::cout << "...resolving function : " << e->op_->get_inner_symbol()->to_native_string() << std::endl;

            // TODO: make this section to loop
            //       generic_function_env has only one scope. should check parent environment.
            auto const& function_env = [&](){
                std::cout << (const_environment_base_ptr)env << std::endl;

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

                /// *************
                auto const& f = overload_solver( argument_type_ids, generic_function_env, e->op_, env );
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
            return function_env->get_return_type_id();
        }




        // function call expression
        RILL_TV_OP( analyzer, ast::call_expression, e, env )
        {
            // push values to context stack and evaluate type environment
            type_id_list_t argument_type_ids;
            for( auto const& val : e->arguments_ )
                argument_type_ids.push_back( dispatch( val, env ) );
            assert( std::count( argument_type_ids.cbegin(), argument_type_ids.cend(), type_id_undefined ) == 0 );


            //       generic_function_env has only one scope. should check parent environment.
            auto const& function_env = [&](){
                std::cout << (const_environment_base_ptr)env << std::endl;

                // TODO: fix lookup phase
                // find a function environment that has same name.
                auto const& target_env = lookup_with_instanciation( env, e->reciever_ );

                // compilation errors
                if ( target_env == nullptr ) {
                    // symbol not found
                    // ?: look up 1 rank top environment or other namespace groups
                    assert( false );
                }
                if ( target_env->get_symbol_kind() != kind::type_value::e_parameter_wrapper ) {
                    // symbol type was not matched
                    assert( false );
                }

                auto const has_parameter_env = std::static_pointer_cast<has_parameter_environment_base>( target_env );
                if ( has_parameter_env->get_inner_symbol_kind() != kind::type_value::e_function ) {
                    // symbol type was not matched
                    assert( false );
                }

                // this environment has all functions that have same identifier
                auto generic_function_env
                    = std::static_pointer_cast<has_parameter_environment<function_symbol_environment>>( has_parameter_env );

                /// *************
                auto const& f = overload_solver_allow_no_entry( argument_type_ids, generic_function_env, e->reciever_->last(), env );

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
                auto const& re_f = overload_solver( argument_type_ids, generic_function_env, e->reciever_->last(), env );
                if ( re_f )
                    return re_f;


                // may be overload failed
                // TODO: dig environment once...

                return re_f/*nullptr*/;
            }();

            // memoize called function env
            std::cout << "memoed" << std::endl;
            function_env->connect_from_ast( e );

            return function_env->get_return_type_id();
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



        //
        //
        //
        RILL_TV_OP( analyzer, ast::intrinsic_value, v, env )
        {
            // look up literal type
            auto const class_env = env->lookup( v->literal_type_name_ );
            assert( class_env != nullptr );  // literal type must exist

            //
            return class_env->make_type_id( class_env, determine_type_attributes() );
        }



        //
        //
        //
        RILL_TV_OP( analyzer, ast::variable_value, v, parent_env )
        {
            std::cout << "Find Var: " << v->variable_name_->last()->get_inner_symbol()->to_native_string() << std::endl;
            auto const& target_env = lookup_with_instanciation( parent_env, v->variable_name_ );
            if ( target_env == nullptr ) {
                // compilation error
                assert( false && "" );
            }

            if ( target_env->get_symbol_kind() != kind::type_value::e_variable ) {
                // symbol type was not matched
                assert( false );
            }

            auto const& variable_env = std::static_pointer_cast<variable_symbol_environment>( target_env );

            // memoize
            std::cout << "memoed" << std::endl;
            variable_env->connect_from_ast( v );

            return variable_env->get_type_id();
/*
            auto const& type_env = parent_env->get_env_at( variable_env->get_type_env_id() ).lock();
            assert( type_env != nullptr );
            std::cout << type_env->mangled_name() << std::endl;
            return type_env;
*/
        }

    } // namespace semantic_analysis
} // namespace rill
