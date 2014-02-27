//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_ANALYZER_INSTANTIATION_TYPE_HPP
#define RILL_SEMANTIC_ANALYSIS_ANALYZER_INSTANTIATION_TYPE_HPP


namespace rill
{
    namespace semantic_analysis
    {
        template<typename Visitor>
        


                //
                // function instanciation
                //

                // make function parameter variable decl
                for( auto const& e : function_ast->get_parameter_list() ) {
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
#if 0
                            if ( type_env->get_symbol_kind() == kind::type_value::e_class ) {
                                auto attr = determine_type_attributes( type_value.attributes );
                                attr <<= e.quality;

                                // declare
                                f_env->parameter_variable_construct(
                                    e.decl_unit.name,
                                    std::dynamic_pointer_cast<class_symbol_environment const>( type_env ),
                                    attr
                                    );

                            } else if ( type_env->get_symbol_kind() == kind::type_value::e_variable ) {
                                assert( false && "invalid" );
//                                // declare
//                                f_env->parameter_variable_construct(
//                                    e.decl_unit.name,
//                                    std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
//                                    attr
//                                    );

                            } else {
                                assert( false );
                            }
#endif
                        } else {
                            // Re prove from template env
                            if ( auto const type_env = lookup_with_instanciation( template_env, type_value.identifiers ) ) {
                                assert( type_env != nullptr );

                                std::cout << "kind: " << debug_string( type_env->get_symbol_kind() ) << std::endl;

                                if ( type_env->get_symbol_kind() == kind::type_value::e_class ) {
                                    auto attr = determine_type_attributes( type_value.attributes );
                                    attr <<= e.quality;

                                    // declare
                                    f_env->parameter_variable_construct(
                                        e.decl_unit.name,
                                        std::dynamic_pointer_cast<class_symbol_environment const>( type_env ),
                                        attr
                                        );

                                } else if ( type_env->get_symbol_kind() == kind::type_value::e_variable ) {
                                    auto const& v_env
                                        = std::dynamic_pointer_cast<variable_symbol_environment const>( type_env );
                                    assert( v_env );

                                    auto const& v_type
                                        = v_env->get_type();

                                    auto const& c_env
                                        = std::dynamic_pointer_cast<class_symbol_environment const>(
                                            visitor->root_env_->get_env_strong_at( v_type.class_env_id )
                                            );
                                    assert( c_env );

                                    // TODO: fix cond
                                    if ( c_env->mangled_name() != "type" ) {
                                        assert( false && "[error] currently non type was not supported..." );
                                    }

                                    if ( visitor->ctfe_engine_->value_holder()->is_defined( v_env->get_id() ) ) {
                                        //
                                        std::cout << "type is determined!!!!1" << std::endl;

                                        auto const& t_detail
                                            = static_cast<type_detail_ptr>(
                                                visitor->ctfe_engine_->value_holder()->ref_value( v_env->get_id() )
                                                );
                                        assert( t_detail != nullptr );

                                        // get environment of arguments
                                        auto const& tt
                                            = visitor->root_env_->get_type_at( t_detail->type_id );

                                        std::cout << "** typeid << " << t_detail->type_id << std::endl
                                                  << "** " << tt.class_env_id << std::endl;;

                                        // TODO: set hint to know that type was determined from template arguments
                                        f_env->parameter_variable_construct(
                                            e.decl_unit.name,
                                            tt.class_env_id,
                                            tt.attributes
                                            );

                                    } else {
                                        // TODO: type inference from type of passed function arguments
                                        assert( false && "[ice] uu..." );
                                    }

//                                    assert(false&&"nyan");
//                                // declare
//                                f_env->parameter_variable_construct(
//                                    e.decl_unit.name,
//                                    std::dynamic_pointer_cast<class_symbol_environment const>( class_env ),
//                                    attr
//                                    );

                                } else {
                                    assert( false );
                                }

                            } else {
                                // type was not found, !! compilation error !!
                                assert( false );
                            }
                        }

                    } else {
                        // type inferenced by result of evaluated [[default initializer expression]]

                        // TODO: implement type inference
                        assert( false );
                    }
                }

                // scan all statements in this function body
                visitor->dispatch( function_ast->inner_, f_env );


                // ?: TODO: use block expression


                // Return type
                if ( function_ast->return_type_ ) {
                    // evaluate constant expresison as type
                    auto const& type_value = interpreter::evaluate_as_type( f_env, *function_ast->return_type_ );

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
                        f_env->complete( return_type_id, function_ast->get_identifier()->get_inner_symbol()->to_native_string() );

                    } else {
                        // type was not found, !! compilation error !!
                        assert( false );
                    }

                } else {
                    // TODO: implement return type inference
                    assert( false && "function return type inference was not supported yet" );
                }

                //
                std::cout << std::endl << "!!add overload!!" << std::endl << std::endl;
                f_env->get_parameter_wrapper_env()->add_overload( f_env );



                f_env->link_with_ast( function_ast );

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_INSTANTIATION_TYPE_HPP*/
