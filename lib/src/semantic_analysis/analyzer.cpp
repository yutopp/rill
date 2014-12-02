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
#include <rill/syntax_analysis/parse.hpp>

#include <rill/utility/colorize.hpp>
#include <rill/utility/tie.hpp>

#include <unordered_map>
#include <cmath>

#include <boost/filesystem.hpp>

#include <boost/scope_exit.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        static auto worse( analyzer::function_match_level const& a, analyzer::function_match_level const& b )
        {
            return static_cast<analyzer::function_match_level>(
                std::max( static_cast<int>( a ), static_cast<int>( b ) )
                );
        }

        static auto better( analyzer::function_match_level const& a, analyzer::function_match_level const& b )
        {
            return static_cast<analyzer::function_match_level>(
                std::min( static_cast<int>( a ), static_cast<int>( b ) )
                );
        }

        static auto is_better_than( analyzer::function_match_level const& a, analyzer::function_match_level const& b )
        {
            return static_cast<int>( a ) < static_cast<int>( b );
        }

        static auto is_better_than_or_equals( analyzer::function_match_level const& a, analyzer::function_match_level const& b )
        {
            return static_cast<int>( a ) <= static_cast<int>( b );
        }


        namespace detail
        {
            inline void mask_type_attribute_by(
                attribute::type_attributes& attr,
                attribute::holder_kind const& k
                )
            {
                attr.quality = k;
            }

            inline void mask_type_attribute_by(
                attribute::type_attributes& attr,
                attribute::modifiability_kind const& k
                )
            {
                if ( k != attribute::modifiability_kind::k_none ) {
                    attr.modifiability = k;
                }
            }

            template<typename T>
            void set_mask_by(
                attribute::type_attributes& attr,
                T const& arg
                )
            {
                mask_type_attribute_by( attr, arg );
            }

            template<typename T, typename... Args>
            void set_mask_by(
                attribute::type_attributes& attr,
                T const& arg,
                Args const&... args
                )
            {
                mask_type_attribute_by( attr, arg );
                set_mask_by( attr, args... );
            }
        } // namespace detail

        inline auto mask_by(
            attribute::type_attributes a,  // copy
            attribute::type_attributes const& b
            )
            -> attribute::type_attributes
        {
            detail::set_mask_by(
                a,
                b.quality,
                b.modifiability
                );

            return a;
        }



        namespace detail
        {
            inline void mask_type_attribute_transitively_by(
                attribute::type_attributes&,
                attribute::holder_kind const&
                )
            {
                // DO nothing
            }

            inline void mask_type_attribute_transitively_by(
                attribute::type_attributes& attr,
                attribute::modifiability_kind const& k
                )
            {
                // immutable > const > mutable
                if ( k != attribute::modifiability_kind::k_none ) {
                    if ( k > attr.modifiability ) {
                        attr.modifiability = k;
                    }
                }
            }

            template<typename T>
            void set_transitive_mask_by(
                attribute::type_attributes& attr,
                T const& arg
                )
            {
                mask_type_attribute_transitively_by( attr, arg );
            }

            template<typename T, typename... Args>
            void set_transitive_mask_by(
                attribute::type_attributes& attr,
                T const& arg,
                Args const&... args
                )
            {
                mask_type_attribute_transitively_by( attr, arg );
                set_transitive_mask_by( attr, args... );
            }
        } // namespace detail

        inline auto mask_transitively(
            attribute::type_attributes a,  // copy
            attribute::type_attributes const& b
            )
            -> attribute::type_attributes
        {
            detail::set_transitive_mask_by(
                a,
                b.quality,
                b.modifiability
                );

            return a;
        }


        analyzer::builtin_class_envs_cache::builtin_class_envs_cache(
            environment_base_ptr const& root_env
            )
        {
            auto install_primitive_class = [&]( std::string const& type_name ) mutable
                {
                    auto env
                        = to_unique_class_env( root_env->lookup( type_name ) );
                    assert( env != nullptr );

                    primitive_cache_[type_name] = env;
                };

            install_primitive_class( "void" );
            install_primitive_class( "type" );
            install_primitive_class( "bool" );
            install_primitive_class( "int8" );
            install_primitive_class( "int" );
            install_primitive_class( "float" );
        }

        inline auto analyzer::builtin_class_envs_cache::find_primitive(
            std::string const& type_name
            ) const
            -> class_symbol_environment_ptr
        {
            auto const it = primitive_cache_.find( type_name );
            assert( it != primitive_cache_.cend() );

            return it->second;
        }


        //
        auto analyzer::get_primitive_class_env( std::string const& type_name )
            -> class_symbol_environment_ptr
        {
            auto env
                = builtin_class_envs_cache_->find_primitive( type_name );

            if ( env->is_incomplete() ) {
                dispatch( env->get_related_ast(), env->get_parent_env() );
            }

            return env;
        }





        //
        analyzer::analyzer(
            global_environment_ptr const& g_env,
            intrinsic_action_holder_ptr const& holder,
            analyzer_options const& options,
            abstract_system_info const& sys_info
            )
            : g_env_( g_env )
            , action_holder_( holder )
            , system_info_( sys_info )
            , type_detail_pool_( std::make_shared<type_detail_pool_t>() )
            , type_detail_factory_(
                std::make_shared<type_detail_factory>( g_env, type_detail_pool_ )
                )
            , ctfe_engine_(
                compile_time::llvm_engine::make_ctfe_engine(
                    this, g_env, holder, type_detail_pool_
                    )
                )
        {
            // append
            for( auto&& p : options.system_import_path ) {
                system_import_path_.push_back( fs::absolute( p ) );
            }
        }


        //
        auto analyzer::ref_type(
            type_detail_ptr const& ty_detail
            ) const -> type const&
        {
            return g_env_->get_type_at( ty_detail->type_id );
        }

        auto analyzer::ref_env(
            type_detail_ptr const& ty_detail
            ) const -> environment_base_ptr
        {
            auto const& ty = ref_type( ty_detail );
            return g_env_->get_env_at_as_strong_ref( ty.class_env_id );
        }

        //
        auto delegate_parent_attributes(
            attribute::type_attributes const& parent_attributes,
            attribute::type_attributes const& child_attributes
            )
            -> attribute::type_attributes
        {
            attribute::type_attributes new_attr = child_attributes;

            // delegate modifiability
            // if parent has strong modifiability than that of child
            if ( parent_attributes.modifiability > child_attributes.modifiability ) {
                new_attr.modifiability = parent_attributes.modifiability;
            }

            return new_attr;
        }


        //
        auto analyzer::qualify_type(
            type_detail_ptr const& ty_detail,
            attribute::type_attributes const& type_attr
            ) -> type_detail_ptr
        {
            // TODO: add duplicate check
            auto const& t = ref_type( ty_detail );

            auto const& qualified_type_id
                = g_env_->make_type_id(
                    g_env_->get_env_at_as_strong_ref<class_symbol_environment>( t.class_env_id ),
                    type_attr
                    );

            return type_detail_pool_->construct(
                qualified_type_id,
                ty_detail->target_env,
                ty_detail->nest,
                ty_detail->template_args
                );
        }


        auto analyzer::declare_template_parameter_variables(
            ast::parameter_list const& template_parameters,
            environment_base_ptr const& inner_env,
            environment_base_ptr const& parent_env
            )
            -> std::vector<variable_symbol_environment_ptr>
        {
            std::vector<variable_symbol_environment_ptr> declared_envs(
                template_parameters.size()
                );

            rill_dout << "TEMPLATE param size is " << template_parameters.size() << std::endl;

            for( std::size_t i=0; i<template_parameters.size(); ++i ) {
                auto const& template_parameter = template_parameters.at( i );
                //
                // 1. declare template parameters
                //
                regard_variable_is_not_already_defined(
                    inner_env,
                    template_parameter.decl_unit.name
                    );

                if ( template_parameter.decl_unit.init_unit.type ) {
                    resolve_type(
                        template_parameter.decl_unit.init_unit.type,
                        attribute::holder_kind::k_ref,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             const_class_symbol_environment_ptr const& class_env
                            )
                        {
                            rill_dout << "Construct template parameter val / index : " << i << std::endl;

                            // declare the template parameter into function env as variable
                            auto const& v_env
                                = inner_env->incomplete_construct(
                                    kind::k_variable,
                                    template_parameter.decl_unit.name
                                    );
                            // TODO: link with ptr to ast
                            v_env->complete( ty_d->type_id );

                            declared_envs[i] = v_env;
                        });

                } else {
                    // deduce this template variable is "type" type.
                    rill_dout << "Construct template parameter[type] val / index : " << i << std::endl;

                    auto const& type_c_env = get_primitive_class_env( "type" );
                    auto const& tid = g_env_->make_type_id(
                        type_c_env,
                        attribute::make(
                            attribute::holder_kind::k_ref,
                            attribute::modifiability_kind::k_immutable
                            )
                        );

                    // declare the template parameter into function env as variable
                    auto const& v_env
                        = inner_env->incomplete_construct(
                            kind::k_variable,
                            template_parameter.decl_unit.name
                            );
                    // TODO: link with ptr to ast
                    v_env->complete( tid );

                    declared_envs[i] = v_env;
                }
            }

            return declared_envs;
        }


        // declare template parameter variable, and substitute explicit argument
        auto analyzer::assign_explicit_template_parameters(
            ast::parameter_list const& template_parameters,
            std::vector<variable_symbol_environment_ptr> const& decl_template_var_envs,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env
            )
            -> bool
        {
            assert( template_args != nullptr );

            // compare template's arguments and parameters
            // TODO: add error process
            assert( template_args->size() <= decl_template_var_envs.size() );

            for( std::size_t i=0; i<template_args->size(); ++i ) {
                auto const& template_parameter = template_parameters.at( i );

                // substitute template arguments
                // save template argument value to the template variables env
                rill_dout << "TEMPLATE ARGS index: " << i << std::endl;
                auto const& template_var_env = decl_template_var_envs.at( i );
                auto const& template_arg = template_args->at( i );

                // DEBUG
                rill_dregion {
                    auto const& tt
                        = g_env_->get_type_at( template_arg.type_id );

                    auto const& c_e
                        = std::static_pointer_cast<class_symbol_environment const>(
                            g_env_->get_env_at_as_strong_ref(
                                tt.class_env_id
                                )
                            );

                    rill_dout << "#### [parameter type]: " << c_e->get_qualified_name() << std::endl;

                    if ( template_arg.is_type() ) {
                        rill_dout << "type: inner value is " << std::endl;
                        auto const& t_detail
                            = static_cast<type_detail_ptr>( template_arg.element );
                        assert( t_detail != nullptr );

                        // get environment of arguments
                        auto const& tt
                            = g_env_->get_type_at( t_detail->type_id );

                        auto const& c_e
                            = std::static_pointer_cast<class_symbol_environment const>(
                                g_env_->get_env_at_as_strong_ref(
                                    tt.class_env_id
                                    )
                                );

                        rill_dout << "** typeid << " << t_detail->type_id << std::endl
                                  << "** " << tt.class_env_id << std::endl;

                        rill_dout << "BINDED " << template_var_env->get_id()
                                  << " -> " << c_e->get_qualified_name() << std::endl;
                    } else {
                        rill_dout << "value: " << std::endl;
                    }
                } // debug

                // type check
                RILL_PP_TIE( level, conv_function_env,
                             try_type_conversion(
                                 template_var_env->get_type_id(),
                                 template_arg.type_id,
                                 parent_env
                                 )
                    );

                // TODO: fix
                switch( level ) {
                case function_match_level::k_exact_match:
                    rill_dout << "Exact" << std::endl;
                    break;
                case function_match_level::k_qualifier_conv_match:
                    rill_dout << "Qual" << std::endl;
                    break;
                case function_match_level::k_implicit_conv_match:
                    rill_dout << "Implicit" << std::endl;
                    return false;
                case function_match_level::k_no_match:
                    rill_dout << "NoMatch" << std::endl;
                    return false;
                }

                // save
                if ( template_arg.is_type() ) {
                    auto const& t_detail
                        = static_cast<type_detail_ptr>( template_arg.element );

                    ctfe_engine_->value_holder()->bind_value(
                        template_var_env->get_id(),
                        t_detail
                        );

                } else {
                    ctfe_engine_->value_holder()->bind_value(
                        template_var_env->get_id(),
                        template_arg.element
                        );
                }
            }

            return true;
        }

        // FIXME: logic
        auto analyzer::make_template_signature_string(
            std::vector<variable_symbol_environment_ptr> const& decl_template_var_envs
            )
            -> std::string
        {
            std::string s;

            for( auto const& var_env : decl_template_var_envs ) {
                auto const& ty
                    = g_env_->get_type_at( var_env->get_type_id() );
                auto const& c_env
                    = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>( ty.class_env_id );
                assert( c_env != nullptr );

                // value bound to this env
                auto const& evaled_value
                    = ctfe_engine_->value_holder()->ref_value( var_env->get_id() );
                assert( evaled_value != nullptr );

                switch( c_env->get_builtin_kind() ) {
                case class_builtin_kind::k_type:
                {
                    auto const& ty_detail
                        = static_cast<type_detail_ptr>( evaled_value );
                    auto const& val_ty
                        = g_env_->get_type_at( ty_detail->type_id );
                    auto const& val_c_env
                        = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>( val_ty.class_env_id );
                    assert( val_c_env != nullptr );
                    s += "T/";
                    s += make_mangled_name( val_c_env, val_ty.attributes );

                    break;
                }

                case class_builtin_kind::k_int32:
                {
                    auto const& num
                        = static_cast<std::int32_t const* const>( evaled_value );
                    s += "I32/";
                    s += std::to_string( *num );

                    break;
                }

                default:
                {
                    rill_dout << c_env->get_base_name() << std::endl;
                    assert( false && "[[ice]] value parameter was not supported yet" );
                }
                } // switch

                s += ":";
            }

            return s;
        }

        //
        auto analyzer::instantiate_class_templates(
            multiple_set_environment_ptr const& multiset_env,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env
            )
            -> std::vector<class_symbol_environment_ptr>
        {
            std::vector<class_symbol_environment_ptr> xc; // TODO: remove

            // solve CLASS template
            for( auto&& env : multiset_env->get_template_environments() ) {
                rill_dout << colorize::standard::fg::red
                          << "!!!! template: " << std::endl
                          << colorize::standard::reset
                          << std::endl;

                //
                auto const& template_ast
                    = std::static_pointer_cast<ast::template_statement>(
                        env->get_related_ast()
                        );
                assert( template_ast != nullptr );


                auto const& class_def_ast
                    = std::static_pointer_cast<ast::class_definition_statement>(
                        template_ast->clone_inner_node()
                        );
                assert( class_def_ast != nullptr );
                assert( class_def_ast->get_identifier() != nullptr );

                // construct incomplete function environment frame
                auto instanting_c_env
                    = multiset_env->allocate_inner_env<class_symbol_environment>(
                        class_def_ast->get_identifier()->get_inner_symbol()->to_native_string()
                        );

                // declare template parameters as variables
                auto const decl_template_var_envs
                    = declare_template_parameter_variables(
                        template_ast->parameter_list_,
                        instanting_c_env,
                        env
                        );

                assert( template_args != nullptr );
                auto const is_succeeded
                    = assign_explicit_template_parameters(
                        template_ast->parameter_list_,
                        decl_template_var_envs,
                        template_args,
                        parent_env
                        );
                assert( is_succeeded && "" );

                // TODO: relocate cache evaluation
                auto const signature_string
                    = make_template_signature_string( decl_template_var_envs );

                rill_dout << "========================================== !!!!! => " << signature_string << std::endl;
                if ( auto const& cache = multiset_env->find_instanced_environments( signature_string ) ) {
                    //
                    auto const& cached_c_env = cast_to<class_symbol_environment>( cache );
                    xc.push_back( cached_c_env );

                    // TODO: remove instanting_c_env

                } else {
                    // collect identifiers
                    if ( class_def_ast->inner_ != nullptr ) {
                        collect_identifier(
                            g_env_,
                            class_def_ast->inner_,
                            instanting_c_env
                            );
                    }

                    //
                    // class instanciation
                    if ( !complete_class( class_def_ast, instanting_c_env, template_args, std::cref( signature_string ) ) ) {
                        // Already completed...
                        // maybe, error...
                        assert( false );
                        continue;
                    }

                    // link
                    instanting_c_env->link_with_ast( class_def_ast );
                    multiset_env->add_to_instanced_environments( instanting_c_env, signature_string );

                    //
                    xc.push_back( instanting_c_env );
                }

                rill_dout << colorize::standard::fg::red
                          << "!!!! END: template: " << std::endl
                          << colorize::standard::reset
                          << std::endl;
            }

            return xc;
        }


        auto analyzer::complete_class(
            ast::class_definition_statement_ptr const& s,
            class_symbol_environment_ptr const& c_env,
            type_detail::template_arg_pointer const& template_args,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature
            )
            -> bool
        {
            // guard double check
            if ( c_env->is_checked() ) {
                rill_dout << "Already, checked" << std::endl;
                // assert( false );
                return false;
            }
            auto const& qualified_name
                = make_qualified_name( c_env, template_signature );
            c_env->check( qualified_name );

            block_envs_.emplace( c_env );
            BOOST_SCOPE_EXIT_ALL(this) {
                block_envs_.pop();
            };

            auto const& attributes
                = s->decl_attr_;

            if ( s->inner_ != nullptr ) {
                // user defined class!

                if ( ( attributes & attribute::decl::k_intrinsic ) != 0 ) {
                    assert( false && "[error] class that has body can not be intrinsic" );
                }

                if ( ( attributes & attribute::decl::k_extern ) != 0 ) {
                    assert( false && "[error] class that has body can not be extern" );
                }

                // set 0 values
                // c_env->set_host_align( 0 );
                // c_env->set_host_size( 0 );
                c_env->set_target_align( 0 );
                c_env->set_target_size( 0 );

                // analyze class body
                dispatch( s->inner_, c_env );

                //
                rill_dout /*<< "host align   : " << c_env->get_host_align() << std::endl
                            << "host size    : " << c_env->get_host_size() << std::endl*/
                          << "target align : " << c_env->get_target_align() << std::endl
                          << "target size  : " << c_env->get_target_size() << std::endl;

                // expect as structured class(not a strong typedef)
                c_env->set_attribute( attribute::decl::k_structured );

                // complete class data
                c_env->complete( attributes );

            } else {
                rill_dout << "builtin class!" << std::endl;

                if ( ( attributes & attribute::decl::k_extern ) != 0 ) {
                    auto const& extern_s
                        = std::static_pointer_cast<ast::extern_class_declaration_statement>( s );
                    if ( ( attributes & attribute::decl::k_intrinsic ) != 0 ) {
                        // call inner action
                        if ( auto&& id = action_holder_->is_registered( extern_s->extern_symbol_name_ ) ) {
                            auto const& action = action_holder_->at( *id );
                            assert( action != nullptr );

                            action->invoke(
                                processing_context::k_semantics_typing,
                                c_env
                                );

                        } else {
                            rill_dout << extern_s->extern_symbol_name_ << std::endl;
                            assert( false && "[error] this intrinsic class is not registered" );
                        }

                        // TODO: change...;(;(;(
                        if ( c_env->get_builtin_kind() == class_builtin_kind::k_array ) {
                            // set special flag as Array
                            // array template args are
                            // [0]: type(no attributes by default)
                            // [1]: number of elements
                            assert( template_args->size() == 2 );
                            assert( template_args->at( 0 ).is_type() );

                            auto const& array_element_ty_detail
                                = static_cast<type_detail_ptr>( template_args->at( 0 ).element );
                            auto ty = g_env_->get_type_at( array_element_ty_detail->type_id ); // make copy
                            ty.attributes = overlap_empty_attr( ty.attributes, attribute::make_default() );
                            auto const& array_element_type_id = g_env_->make_type_id(
                                ty.class_env_id,
                                ty.attributes
                                );

                            auto const& array_element_num
                                = static_cast<std::int32_t const* const>( template_args->at( 1 ).element );

                            rill_dout << "Array num is " << *array_element_num << std::endl;
                            c_env->make_as_array(
                                array_element_type_id,
                                *array_element_num
                                );

                            // TODO: set kind
                            // class_traits_kind::k_has_non_trivial_copy_ctor;
                            // class_traits_kind::k_has_non_trivial_move_ctor;
                            // class_traits_kind::k_has_non_trivial_copy_assign;
                            // class_traits_kind::k_has_non_trivial_move_assign;
                            // class_traits_kind::k_has_non_trivial_dtor;
                            // class_traits_kind::k_has_non_trivial_copy_ctor;
                            // class_traits_kind::k_has_non_default_copyable_member;

                            // TODO: set alignment, size
                            // c_env->set_target_align(
                            // c_env->set_target_size(

                        } else if ( c_env->get_builtin_kind() == class_builtin_kind::k_ptr ) {
                            // set special flag as Pointer
                            // ptr template args are
                            // [0]: type(no attributes by default)
                            assert( template_args->size() == 1 );
                            assert( template_args->at( 0 ).is_type() );

                            auto const& ptr_element_ty_detail
                                = static_cast<type_detail_ptr>( template_args->at( 0 ).element );
                            auto ty = g_env_->get_type_at( ptr_element_ty_detail->type_id ); // make copy
                            ty.attributes = overlap_empty_attr( ty.attributes, attribute::make_default() );
                            auto const& ptr_element_type_id = g_env_->make_type_id(
                                ty.class_env_id,
                                ty.attributes
                                );

                            rill_dout << "This is ptr!" << std::endl;
                            c_env->make_as_pointer( ptr_element_type_id );
                        }

                    } else {
                        assert( false && "[error]" );
                    }

                } else {
                    assert( false && "[error]" );
                }

                // complete class data
                c_env->complete( attributes );
            }

            return true;
        }


        auto analyzer::solve_class_candidate(
            multiple_set_environment_ptr const& multiset_env,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env
            )
            -> class_symbol_environment_ptr
        {
            assert( multiset_env->get_representation_kind() == kind::type_value::e_class );

            rill_dout << colorize::standard::fg::red
                      << "!!!! class solving: " << multiset_env->get_name() << std::endl
                      << "!!!! templated class candidate num: " << multiset_env->get_template_environments().size()
                      << colorize::standard::reset
                      << std::endl;

            assert( template_args != nullptr );

            // TODO: change selectiong logic
            auto const& xc
                = instantiate_class_templates( multiset_env, template_args, parent_env );

            rill_dout << "class candidate: " << xc.size() << std::endl;
            assert( xc.size() != 0 && "no candidate" );
            assert( xc.size() == 1 && "ambigous" );

            // TODO: fix
            return xc[0];
        }






        auto analyzer::evaluate_template_args(
            ast::expression_list const& arguments,
            environment_base_ptr const& parent_env
            )
            -> type_detail::template_arg_type
        {
            rill_dout << "eval template arguments!!!" << std::endl;

            // evaluate template arguments...
            type_detail::template_arg_type template_args;

            // TODO: implement
            for( auto const& expression : arguments ) {
                //
                rill_dout << "template expresison!!!!!!" << std::endl;
                auto const& argument_ty_detail = dispatch( expression, parent_env );

                if ( !is_type_id( argument_ty_detail->type_id ) ) {
                    rill_ice( "not type id is not suppurted yet" );
                }

                // get environment of arguments
                auto const& ty
                    = g_env_->get_type_at( argument_ty_detail->type_id );

                auto const& c_env
                    = std::static_pointer_cast<class_symbol_environment const>(
                        g_env_->get_env_at_as_strong_ref( ty.class_env_id )
                        );
                assert( c_env != nullptr );

                // eval expression of arguments
                auto const& argument_evaled_value
                    = ctfe_engine_->execute_as_raw_storage( expression, parent_env );
                assert( argument_evaled_value != nullptr );

                //
                auto ta = [&]() -> type_detail::dependent_type {
                    switch( c_env->get_builtin_kind() ) {
                    case class_builtin_kind::k_type:
                    {
                        auto const& c_env = get_primitive_class_env( "type" );
                        return {
                            argument_ty_detail,
                            static_cast<type_detail_ptr>( argument_evaled_value ),
                            dependent_value_kind::k_type,
                            g_env_->make_type_id(
                                c_env,
                                attribute::make(
                                    attribute::holder_kind::k_val,
                                    attribute::modifiability_kind::k_immutable
                                    )
                                )
                        };
                    }

                    case class_builtin_kind::k_int8:
                    {
                        auto const& c_env = get_primitive_class_env( "int8" );
                        return {
                            argument_ty_detail,
                            argument_evaled_value,
                            dependent_value_kind::k_int8,
                            g_env_->make_type_id(
                                c_env,
                                attribute::make(
                                    attribute::holder_kind::k_val,
                                    attribute::modifiability_kind::k_immutable
                                    )
                                )
                        };
                    }

                    case class_builtin_kind::k_int32:
                    {
                        auto const& c_env = get_primitive_class_env( "int" );
                        return {
                            argument_ty_detail,
                            argument_evaled_value,
                            dependent_value_kind::k_int32,
                            g_env_->make_type_id(
                                c_env,
                                attribute::make(
                                    attribute::holder_kind::k_val,
                                    attribute::modifiability_kind::k_immutable
                                    )
                                )
                        };
                    }

                    default:
                    {
                        rill_dout << c_env->get_base_name() << std::endl;
                        assert( false && "[[ice]] value parameter was not supported yet" );
                        return { nullptr, nullptr, dependent_value_kind::k_none };
                    }
                    } // switch
                }();
                rill_dout << "argt: " << c_env->get_qualified_name() << std::endl;

                template_args.push_back( ta );
            }

            return template_args;
        }





        //
        auto qualifier_conversion_from_ref(
            attribute::type_attributes const& parameter_attributes,
            attribute::type_attributes const& argument_attributes,
            const_class_symbol_environment_ptr const& argument_c_env
            )
            -> boost::optional<attribute::type_attributes>
        {
            auto result_attr = argument_attributes;

            switch( parameter_attributes.quality )  // to
            {
            case attribute::holder_kind::k_ref:
            {
                // ref to ref
                switch( argument_attributes.modifiability ) {
                case attribute::modifiability_kind::k_immutable:
                case attribute::modifiability_kind::k_none: // none == immutable
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_none: // none == immutable
                    case attribute::modifiability_kind::k_const:
                        break;
                    case attribute::modifiability_kind::k_mutable:
                        return boost::none;
                    default:
                        assert( false );
                    } // switch( parameter_attributes.attributes.modifiability )
                    break;

                case attribute::modifiability_kind::k_const:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_none: // none == immutable
                        return boost::none;
                    case attribute::modifiability_kind::k_const:
                        break;
                    case attribute::modifiability_kind::k_mutable:
                        return boost::none;
                    default:
                        assert( false );
                    } // switch( parameter_attributes.attributes.modifiability )
                    break;

                case attribute::modifiability_kind::k_mutable:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_none: // none == immutable
                        return boost::none;
                    case attribute::modifiability_kind::k_const:
                    case attribute::modifiability_kind::k_mutable:
                        break;
                    default:
                        assert( false );
                    } // switch( target_type.attributes.modifiability )
                    break;

                default:
                    assert( false );
                } // switch( parameter_attributes.modifiability )

                result_attr <<= attribute::holder_kind::k_ref;
                break;
            }

            case attribute::holder_kind::k_val:
            {
                // ref to val
                switch( parameter_attributes.modifiability ) {
                case attribute::modifiability_kind::k_immutable:
                case attribute::modifiability_kind::k_none: // none == immutable
                    if ( !argument_c_env->is_immutable() ) {
                        return boost::none;
                    }
                    break;
                case attribute::modifiability_kind::k_const:
                case attribute::modifiability_kind::k_mutable:
                    break;
                default:
                    assert( false );
                } // switch( parameter_attributes.modifiability )

                result_attr <<= attribute::holder_kind::k_val;
                break;
            }

            default:
                assert( false );
            }

            return result_attr;
        }

        //
        auto qualifier_conversion_from_val(
            attribute::type_attributes const& parameter_attributes,
            attribute::type_attributes const& argument_attributes,
            const_class_symbol_environment_ptr const& argument_c_env
            )
            -> boost::optional<attribute::type_attributes>
        {
            auto result_attr = argument_attributes;

            switch( parameter_attributes.quality )  // to
            {
            case attribute::holder_kind::k_ref:
            {
                // val to ref
                rill_dout << "val to ref" << std::endl;
                switch( argument_attributes.modifiability ) {
                case attribute::modifiability_kind::k_immutable:
                case attribute::modifiability_kind::k_none: // none == immutable
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_none: // none == immutable
                    case attribute::modifiability_kind::k_const:
                        break;
                    case attribute::modifiability_kind::k_mutable:
                        return boost::none;
                    default:
                        assert( false );
                    }
                    break;

                case attribute::modifiability_kind::k_const:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_none: // none == immutable
                    case attribute::modifiability_kind::k_const:
                        break;
                    case attribute::modifiability_kind::k_mutable:
                        return boost::none;
                    default:
                        assert( false );
                    }
                    break;

                case attribute::modifiability_kind::k_mutable:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_none: // none == immutable
                        return boost::none;
                    case attribute::modifiability_kind::k_const:
                    case attribute::modifiability_kind::k_mutable:
                        break;
                    default:
                        assert( false );
                    }
                    break;

                default:
                    assert( false );
                }

                result_attr <<= attribute::holder_kind::k_ref;
                break;
            }

            case attribute::holder_kind::k_val:
            {
                // val to val
                switch( parameter_attributes.modifiability ) {
                case attribute::modifiability_kind::k_immutable:
                case attribute::modifiability_kind::k_none: // none == immutable
                    if ( !argument_c_env->is_immutable() ) {
                        return boost::none;
                    }
                    break;
                case attribute::modifiability_kind::k_const:
                case attribute::modifiability_kind::k_mutable:
                    break;
                default:
                    assert( false );
                } // switch( parameter_attributes.modifiability )
                break;
            }

            default:
                assert( false );
            }

            return result_attr;
        }

        //
        auto qualifier_conversion(
            attribute::type_attributes const& parameter_attributes,
            attribute::type_attributes const& argument_attributes,
            const_class_symbol_environment_ptr const& argument_c_env
            )
            -> boost::optional<attribute::type_attributes>
        {
            //
            switch( argument_attributes.quality ) // from
            {
            case attribute::holder_kind::k_ref:
            case attribute::holder_kind::k_suggest:
                return qualifier_conversion_from_ref(
                    parameter_attributes,
                    argument_attributes,
                    argument_c_env
                    );

            case attribute::holder_kind::k_val:
                return qualifier_conversion_from_val(
                    parameter_attributes,
                    argument_attributes,
                    argument_c_env
                    );

            default:
                assert( false );
            }

            return boost::none;
        }


        auto analyzer::infer_param_type_from_arg_type(
            type_detail_ptr const param_type_detail,
            const_type_detail_ptr const arg_type_detail
            )
            -> type_detail_ptr
        {
            auto const& arg_type = g_env_->get_type_at( arg_type_detail->type_id );

            if ( param_type_detail->template_args == nullptr ) {
                // simple type match
                rill_dout << " ? simple type match" << std::endl;
                auto const& param_type = g_env_->get_type_at( param_type_detail->type_id );

                if ( param_type.is_incomplete() ) {
                    // determine this type_detail has a pointer to the template param env
                    rill_dout << " ? : template param" << std::endl;

                    auto const& r_tv_env = param_type_detail->target_env;
                    assert( r_tv_env != nullptr );
                    assert( r_tv_env->get_symbol_kind() == kind::type_value::e_variable );
                    auto const& template_var_env = std::static_pointer_cast<variable_symbol_environment>( r_tv_env );

                    auto ty_d = reinterpret_cast<type_detail*>(
                        ctfe_engine_->value_holder()->ref_value( template_var_env->get_id() )
                        );
                    assert( ty_d != nullptr );

                    auto const new_parameter_val_type_attr
                        = overlap_empty_attr(
                            arg_type.attributes - param_type.attributes,
                            attribute::make_default()
                            );
                    auto const new_paramater_val_type_id
                        = g_env_->make_type_id( arg_type.class_env_id, new_parameter_val_type_attr );
                    rill_dout << "new_parameter_val_type_attr: " << new_parameter_val_type_attr << std::endl;

                    // update value
                    ty_d->type_id = new_paramater_val_type_id;

                    // make "parameter"'s type
                    auto const new_parameter_type_attr
                        = mask_by( new_parameter_val_type_attr, param_type.attributes );
                    auto const new_paramater_type_id
                        = g_env_->make_type_id( arg_type.class_env_id, new_parameter_type_attr );
                    rill_dout << "new_parameter_type_attr: " << new_parameter_type_attr << std::endl;

                    return type_detail_pool_->construct(
                        new_paramater_type_id,
                        nullptr                 // unused
                        );

                } else {
                    // type is defined, so skip and pass to solve phase
                    return param_type_detail;
                }

            } else {
                // needs type pattern match
                assert( false && "currentry, type pattern match was not supported." );
            }

            return nullptr;
        }

        auto analyzer::pointer_qualifier_conversion(
            attribute::type_attributes const& extensive_target_attr,
            type_id_t const& target_type_id,
            attribute::type_attributes const& extensive_current_attr,
            type_id_t const& current_type_id
            )
            -> bool
        {
            auto target_type = g_env_->get_type_at( target_type_id );
            auto const& target_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    target_type.class_env_id
                    );
            target_type.attributes
                = mask_transitively( target_type.attributes, extensive_target_attr );
            target_type.attributes.quality = attribute::holder_kind::k_ref;

            auto current_type = g_env_->get_type_at( current_type_id );
            auto const& current_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    current_type.class_env_id
                    );
            current_type.attributes
                = mask_transitively( current_type.attributes, extensive_current_attr );
            current_type.attributes.quality = attribute::holder_kind::k_ref;

            if ( target_c_env->is_pointer() && current_c_env->is_pointer() ) {
                // both pointer
                return pointer_qualifier_conversion(
                    target_type.attributes,
                    target_c_env->get_pointer_detail()->inner_type_id,
                    current_type.attributes,
                    current_c_env->get_pointer_detail()->inner_type_id
                    );

            } else if ( !target_c_env->is_pointer() && !current_c_env->is_pointer() ) {
                rill_dout << "target=" << std::endl
                          << target_type.attributes
                          << "current=" << std::endl
                          << current_type.attributes
                    ;

                // both value
                return qualifier_conversion(
                    target_type.attributes,
                    current_type.attributes,
                    current_c_env
                    ) != boost::none;

            } else {
                return false;
            }
        }

        // returns (conv_level, conv_function_env)
        auto analyzer::try_type_conversion(
            type_id_t const& param_type_id,
            type_id_t const& arg_type_id,
            environment_base_ptr const& parent_env
            )
            -> std::tuple<
                analyzer::function_match_level,
                function_symbol_environment_ptr
            >
        {
            if ( param_type_id == arg_type_id ) {
                // exact match
                return std::make_tuple(
                    function_match_level::k_exact_match,
                    nullptr
                    );

            } else {
                // try to type conversion
                auto const& param_type = g_env_->get_type_at( param_type_id );
                auto const& param_c_env
                    = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                        param_type.class_env_id
                        );

                auto const& arg_type = g_env_->get_type_at( arg_type_id );
                auto const& arg_c_env
                    = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                        arg_type.class_env_id
                        );

                // special treatment for pointer
                if ( param_c_env->is_pointer() && arg_c_env->is_pointer() ) {
                    // check the qual of pointer "value"
                    if ( qualifier_conversion(
                             param_type.attributes,
                             arg_type.attributes,
                             arg_c_env
                             )
                        )
                    {
                        if( pointer_qualifier_conversion(
                                param_type.attributes,
                                param_c_env->get_pointer_detail()->inner_type_id,
                                arg_type.attributes,
                                arg_c_env->get_pointer_detail()->inner_type_id
                                )
                            )
                        {
                            return std::make_tuple(
                                function_match_level::k_qualifier_conv_match,
                                nullptr
                                );
                        }
                    }

                } else if ( param_type.class_env_id == arg_type.class_env_id ) {
                    // same class, so check quarity conversion
                    if ( qualifier_conversion(
                             param_type.attributes,
                             arg_type.attributes,
                             arg_c_env
                             )
                        )
                    {
                        // qualifier conversion match
                        return std::make_tuple(
                            function_match_level::k_qualifier_conv_match,
                            nullptr
                            );
                    }

                } else {
                    // TODO: implement implicit conversion match(k_implicit_conv_match)
                    // TODO: call constructor

                    // trough


                    // currentry failed immediately

                }
            }

            return std::make_tuple(
                function_match_level::k_no_match,
                nullptr
                );
        }

        // if return value is nullptr, failed to select the function
        auto analyzer::select_suitable_function(
            std::vector<environment_base_ptr> const& enviroments,
            std::vector<type_detail_ptr> const& arg_types,
            environment_base_ptr const& parent_env
            )
            -> std::tuple<
                analyzer::function_match_level,
                boost::optional<std::vector<function_symbol_environment_ptr>>
            >
        {
            auto print_type = [&]( type_id_t const& tid ){
                auto const ty = g_env_->get_type_at( tid );
                auto const& c_env = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    ty.class_env_id
                    );
                rill_dout
                    << "type: " << c_env->get_qualified_name()
                    << " / ty.class_env_id: " << ty.class_env_id << std::endl
                    << "type: " << debug_string( c_env->get_symbol_kind() ) << std::endl
                    << ty.attributes;
            };

            //
            type_id_list_t holder( arg_types.size() );
            std::vector<function_symbol_environment_ptr> f_candidate_envs;

            std::multimap<int, function_symbol_environment_ptr> solved_function_envs;

            // TODO: add hit cache

            auto best_matched_level = function_match_level::k_no_match;

            // first, see normal envs
            for( auto&& env : enviroments ) {
                auto const& f_env = cast_to<function_symbol_environment>( env );
                assert( f_env != nullptr );

                // complate the incomplete function
                if ( f_env->is_incomplete() ) {
                    dispatch( f_env->get_related_ast(), f_env->get_parent_env() );
                }

                // DEBUG
                rill_dout << "[overloads] " << f_env->get_mangled_name() << " ... is_checked ? " << f_env->is_checked() << std::endl
                          << (const_environment_base_ptr)f_env << std::endl;
                assert( f_env->is_checked() );

                auto const& f_env_parameter_type_ids = f_env->get_parameter_type_ids();

                // not matched: argument size is different
                // TODO: support variadic params
                if ( f_env_parameter_type_ids.size() != arg_types.size() ) {
                    continue;
                }

                // assume the function is "exact match"
                auto function_match_level = function_match_level::k_exact_match;
                // type_id_list_t type_id_holder( arg_types.size() );

                // check each arguments/parameters
                for( std::size_t i=0; i<arg_types.size(); ++i ) {
                    auto const& param_type_id = f_env_parameter_type_ids[i];
                    auto const& arg_type_id = arg_types[i]->type_id;

                    rill_dout << "Param Type Conv (" << i << "): ";

                    RILL_PP_TIE( level, conv_function_env,
                                 try_type_conversion(
                                     param_type_id,
                                     arg_type_id,
                                     parent_env
                                     )
                        );

                    rill_dout << ": from" << std::endl;
                    print_type( arg_type_id );
                    rill_dout << ": to" << std::endl;
                    print_type( param_type_id );

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
                        break;
                    }

                    // update level
                    function_match_level = worse( function_match_level, level );

                    // this function will be never matched, so stop to try matching
                    if ( function_match_level == function_match_level::k_no_match ) {
                        break;
                    }
                } // for( match per args )

                // not matched: failed
                if ( function_match_level == function_match_level::k_no_match ) {
                    continue;
                }

                if ( is_better_than( function_match_level, best_matched_level ) ) {
                    // reset candidate set
                    f_candidate_envs.clear();
                }
                best_matched_level = better( best_matched_level, function_match_level );
                if ( function_match_level == best_matched_level ) {
                    f_candidate_envs.push_back( f_env );
                }
            } // for [normal environment]

            rill_dout << " !== overload ======================================================" << std::endl
                      << "best match: " << static_cast<int>( best_matched_level ) << std::endl;

            rill_dregion {
                for( auto&& env : f_candidate_envs ) {
                    std::cout << std::endl
                              << "  !!!!! condidate found >>> " << std::endl
                              << std::endl;
                }
            }

            // check overloaded set

            //
            if ( f_candidate_envs.size() > 0 ) {
                // return best match function
                return std::make_tuple(
                    best_matched_level,
                    std::move( f_candidate_envs )
                    );

            } else {
                return std::make_tuple(
                    function_match_level::k_no_match,
                    boost::none
                    );
            }
        }

        auto analyzer::instantiate_function_templates(
            multiple_set_environment_ptr const& multiset_env,
            std::vector<type_detail_ptr> const& arg_types,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env,
            boost::optional<std::reference_wrapper<std::vector<environment_base_ptr>>> const& instanced_envs
            )
            -> void
        {
            // solve FUNCTION template
            for( auto&& env : multiset_env->get_template_environments() ) {
                rill_dout << colorize::standard::fg::red
                          << "!!!! template: " << std::endl
                          << colorize::standard::reset
                          << std::endl;

                //
                auto const& template_ast
                    = std::static_pointer_cast<ast::template_statement>(
                        env->get_related_ast()
                        );
                assert( template_ast != nullptr );

                // TODO: support "member function" and "extern function"
                auto const& function_def_ast
                    = std::static_pointer_cast<ast::function_definition_statement_base>(
                        template_ast->clone_inner_node()
                        );
                assert( function_def_ast != nullptr );
                assert( function_def_ast->get_identifier() != nullptr );

                // construct incomplete function emvironment frame
                auto instanting_f_env
                    = multiset_env->allocate_inner_env<function_symbol_environment>(
                        function_def_ast->get_identifier()->get_inner_symbol()->to_native_string()
                        );

                // ==========
                // declare template parameters as variables
                auto const decl_template_var_envs
                    = declare_template_parameter_variables(
                        template_ast->parameter_list_,
                        instanting_f_env,
                        env
                        );

                if ( template_args != nullptr ) {
                    // template args are provided
                    auto const is_succeeded
                        = assign_explicit_template_parameters(
                            template_ast->parameter_list_,
                            decl_template_var_envs,
                            template_args,
                            parent_env
                            );
                    assert( is_succeeded && "" );
                }

                auto const arg_index_until_provided
                    = template_args != nullptr
                    ? template_args->size()
                    : 0;
                for( std::size_t i=arg_index_until_provided; i<decl_template_var_envs.size(); ++i ) {
                    // assign empty type value
                    rill_dout << "= Assign empty type value / index : " << i << std::endl;

                    auto const& template_var_env = decl_template_var_envs.at( i );

                    ctfe_engine_->value_holder()->bind_value(
                        template_var_env->get_id(),
                        type_detail_pool_->construct(
                            g_env_->make_type_id(),     // empty type
                            template_var_env            // link to template env
                            )
                        );
                }

                //
                std::vector<type_detail_ptr> presetted_param_types
                    = make_function_parameters_type_details(
                        instanting_f_env,
                        function_def_ast
                        );

                // type decuce!
                assert( presetted_param_types.size() == arg_types.size() );
                for( std::size_t i=0; i<presetted_param_types.size(); ++i ) {
                    rill_dout << "Template arg/param type inference... / index : " << i << std::endl;
                    auto& param_type = presetted_param_types[i];
                    auto const& arg_type = arg_types[i];

                    auto new_ty_d = infer_param_type_from_arg_type( param_type, arg_type );
                    rill_dout << " == is_succeeded : " << ( new_ty_d != nullptr ) << std::endl;
                    if ( new_ty_d == nullptr ) {
                        // TODO: fail! skip this function instatiation
                        assert( false && "" );
                    }

                    // update
                    param_type = new_ty_d;
                }

                // TODO: check existance of incomplete template vaiables

                // TODO: relocate cache evaluation
                auto const signature_string
                    = make_template_signature_string( decl_template_var_envs );

                rill_dout << "========================================== !!!!! => " << signature_string << std::endl;
                if ( auto const& cache = multiset_env->find_instanced_environments( signature_string ) ) {
                    // this function is already instanced, so do nothing
                    // TODO: remove 'instanting_f_env' and function_def_ast

                    if ( auto& o = instanced_envs ) {
                        o->get().push_back( cache );
                    }

                } else {
                    block_envs_.emplace( instanting_f_env );
                    BOOST_SCOPE_EXIT_ALL(this) {
                        block_envs_.pop();
                    };

                    //
                    instanting_f_env->change_progress_to_checked();

                    // new instanced function
                    declare_function_parameters_from_list(
                        instanting_f_env,
                        function_def_ast,
                        presetted_param_types
                        );

                    // return type
                    if ( function_def_ast->return_type_ ) {
                        // if return type was specified, decide type to it.
                        resolve_type(
                            function_def_ast->return_type_,
                            attribute::holder_kind::k_val, // TODO: fix
                            instanting_f_env,
                            [&]( type_detail_ptr const& return_ty_d,
                                 type const& ty,
                                 const_class_symbol_environment_ptr const& class_env
                                )
                            {
                                instanting_f_env->decide_return_type( return_ty_d->type_id );
                            });
                    }

                    // semantic analyze all statements in this function body
                    dispatch( function_def_ast->inner_, instanting_f_env );

                    // Return type
                    solve_function_return_type_semantics( instanting_f_env );

                    //
                    instanting_f_env->complete(
                        make_mangled_name(
                            g_env_,
                            instanting_f_env,
                            std::cref( signature_string )
                            )
                        );
                    instanting_f_env->link_with_ast( function_def_ast );

                    //
                    multiset_env->add_to_instanced_environments( instanting_f_env, signature_string );

                    //
                    if ( auto& o = instanced_envs ) {
                        o->get().push_back( instanting_f_env );
                    }
                }

                rill_dout << colorize::standard::fg::red
                          << "!!!! END: template: " << std::endl
                          << colorize::standard::reset
                          << std::endl;
            }
        }

        auto analyzer::solve_function_return_type_semantics(
            function_symbol_environment_ptr const& f_env
            )
            -> void
        {
            if ( f_env->is_return_type_decided() ) {
                if ( !f_env->is_closed() ) {
                    if ( function_returns_value( f_env ) ) {
                        assert( false && "[error] there is flow doesn't return value" );
                    }
                }

            } else {
                // assert( false && "[Error] return type was not determined..." );
                // Force set "void"
                auto const& void_class_env = get_primitive_class_env( "void" );
                auto ret_ty_id = g_env_->make_type_id(
                    void_class_env,
                    attribute::make_default_type_attributes()
                    );
                f_env->decide_return_type( ret_ty_id );
            }
        }

        auto analyzer::function_returns_value(
            function_symbol_environment_ptr const& f_env
            )
            -> bool
        {
            assert( f_env->is_return_type_decided() );
            auto const& ty = g_env_->get_type_at( f_env->get_return_type_id() );

            return ty.class_env_id != get_primitive_class_env( "void" )->get_id();
        }

        // solve the function from overload set and returns one function environment
        auto analyzer::solve_function_overload(
            multiple_set_environment_ptr const& set_env,
            std::vector<type_detail_ptr> const& arg_types,
            type_detail::template_arg_pointer const& template_args,
            ast::expression_ptr const& e,
            environment_base_ptr const& parent_env
            )
            -> function_symbol_environment_ptr
        {
            //
            assert( set_env->get_representation_kind() == kind::type_value::e_function );

            rill_dout << colorize::standard::fg::red
                      << "!!!! overload solving: " << set_env->get_name() << std::endl
                      << "!!!! normal function candidate num: " << set_env->get_normal_environments().size()
                      << colorize::standard::reset
                      << std::endl;

            if ( template_args == nullptr ) {
                // solve [normal/templated] functions
                // 1. find from normal functions
                auto const n_match_n_f_env = select_suitable_function(
                    set_env->get_normal_environments(),
                    arg_types,
                    parent_env
                    );
                auto const& n_match = std::get<0>( n_match_n_f_env );
                auto const& o_n_f_envs = std::get<1>( n_match_n_f_env );

                // 2. instantiate function templates
                instantiate_function_templates( set_env, arg_types, template_args, parent_env );

                // 3. find from templated functions
                auto const t_match_t_f_env = select_suitable_function(
                    set_env->get_instanced_environments(),
                    arg_types,
                    parent_env
                    );
                auto const& t_match = std::get<0>( t_match_t_f_env );
                auto const& o_t_f_envs = std::get<1>( t_match_t_f_env );

                auto const& res_match = better( n_match, t_match );
                auto const& o_res_f_envs
                    = is_better_than_or_equals( n_match, t_match )
                    ? o_n_f_envs
                    : o_t_f_envs;

                if ( res_match == function_match_level::k_no_match ) {
                    // debug
                    for( auto&& a : arg_types ) {
                        auto const& t
                            = g_env_->get_type_at( a->type_id );
                        assert( t.class_env_id != environment_id_undefined );

                        auto const& t_c_env
                            = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>( t.class_env_id );
                        assert( t_c_env != nullptr );

                        rill_dout << t_c_env->get_qualified_name() << t.attributes << std::endl;
                    }

                    // Error
                    semantic_error(
                        message_code::e_overload_nomatch,
                        e,
                        format( "Overload resolution: suitable function was not found" )
                        );
                }

                if ( o_res_f_envs->size() != 1 ) {
                    semantic_error(
                        message_code::e_overload_anbigous,
                        e,
                        format( "Overload resolution: callable functions were anbigous" )
                        );
                }

                return o_res_f_envs->at( 0 );

            } else {
                // solve only templated functions
                // 1. instantiate function templates
                std::vector<environment_base_ptr> instanced;
                instantiate_function_templates( set_env, arg_types, template_args, parent_env, std::ref( instanced ) );

                // 2. find from templated functions
                auto const t_match_t_f_env = select_suitable_function(
                    instanced,
                    arg_types,
                    parent_env
                    );
                auto const& res_match = std::get<0>( t_match_t_f_env );
                auto const& o_res_f_envs = std::get<1>( t_match_t_f_env );

                if ( res_match == function_match_level::k_no_match ) {
                    semantic_error(
                        message_code::e_overload_nomatch,
                        e,
                        format( "Overload resolution: suitable function was not found" )
                        );
                }

                if ( o_res_f_envs->size() != 1 ) {
                    semantic_error(
                        message_code::e_overload_anbigous,
                        e,
                        format( "Overload resolution: callable functions were anbigous" )
                        );
                }

                return o_res_f_envs->at( 0 );
            }


            return nullptr;
        }


        // for Identifier
        auto analyzer::solve_identifier(
            ast::const_identifier_value_base_ptr const& id,
            environment_base_ptr const& parent_env,
            bool const do_lookup,
            kind::type_value const& exclude_env_type
            ) -> type_detail_ptr
        {
            return id->is_template()
                ? solve_identifier(
                    std::static_pointer_cast<ast::template_instance_value const>( id ),
                    parent_env,
                    do_lookup,
                    exclude_env_type
                    )
                : solve_identifier(
                    std::static_pointer_cast<ast::identifier_value const>( id ),
                    parent_env,
                    do_lookup,
                    exclude_env_type
                    )
                ;
        }


        // for Identifier
        auto analyzer::solve_identifier(
            ast::const_identifier_value_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_lookup,
            kind::type_value const& exclude_env_type
            ) -> type_detail_ptr
        {
            return generic_solve_identifier(
                identifier,
                parent_env,
                do_lookup,
                exclude_env_type
                );
        }


        // for Template Instance Identifier
        auto analyzer::solve_identifier(
            ast::const_template_instance_value_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_lookup,
            kind::type_value const& exclude_env_type
            ) -> type_detail_ptr
        {
            auto const ty_detail
                = generic_solve_identifier(
                    identifier,
                    parent_env,
                    do_lookup,
                    exclude_env_type
                    );
            if ( ty_detail == nullptr ) {
                // propagate nullptr...
                return nullptr;
            }

            // v. template_argument()

            rill_dout << "  ==" << std::endl
                      << "eval template arguments!!!" << std::endl
                      << "  ==" << std::endl
                      << std::endl;

            // evaluate template arguments
            type_detail::template_arg_type template_args
                = evaluate_template_args(
                    identifier->template_argument(),
                    parent_env
                    );

            // set evaluated template args
            if ( ty_detail->template_args == nullptr ) {
                ty_detail->template_args = std::make_shared<type_detail::template_arg_type>();
            }
            (*ty_detail->template_args) = std::move( template_args );

            // class template instantiation!!!
            if ( is_nontype_id( ty_detail->type_id ) ) {
                if ( ty_detail->type_id == (type_id_t)type_id_nontype::e_template_class ) {
                    // returns instances class symbol environment
                    auto const i_c_env = solve_class_candidate(
                        cast_to<multiple_set_environment>( ty_detail->target_env ),
                        ty_detail->template_args,
                        parent_env
                        );

                    // return type should be "type"
                    auto const& type_class_env = get_primitive_class_env( "type" );
                    assert( type_class_env != nullptr );  // literal type must exist

                    ty_detail->type_id
                        = g_env_->make_type_id(
                            type_class_env,
                            attribute::make_default_type_attributes()
                            );

                    // !! important
                    // memoize
                    rill_dout << "()memoed.template_class" << std::endl;
                    i_c_env->connect_from_ast( identifier );
                }
            }

            return ty_detail;
        }


        //
        auto analyzer::generic_solve_identifier(
            ast::const_identifier_value_base_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_lookup,
            kind::type_value const& exclude_env_type
            )
            -> type_detail_ptr
        {
            // TODO: check that identifier is from root

            rill_dout << "## Finding Identifier: " << identifier->get_inner_symbol()->to_native_string() << std::endl
                      << "## astid: " << identifier->get_id() << std::endl;

            bool is_captured = false;
            bool is_capture_enabled = true;

            auto const found_env
                = [&]() {
                    auto env = parent_env->find_on_env( identifier );
                    if ( env ) return env;

                    if ( do_lookup ) {
                        rill_dout << "::RE" << std::endl;
                        assert( parent_env->has_parent() );
                        auto env
                            = parent_env->get_parent_env()->lookup(
                                identifier,
                                exclude_env_type
                                );
                        if ( env ) {
                            is_captured = true;
                            return env;
                        }
                    }

                    return environment_base_ptr{ nullptr };
                }();
            if ( found_env == nullptr ) {
                // identifier was not found, return nullptr
                return nullptr;
            }

            rill_dout << "## kind: " << debug_string( found_env->get_symbol_kind() ) << std::endl
                      << (const_environment_base_ptr)parent_env << std::endl;

            switch( found_env->get_symbol_kind() ) {
            case kind::type_value::e_multi_set:
            {
                auto const& multiset_env = cast_to<multiple_set_environment>( found_env );

                //
                switch( multiset_env->get_representation_kind() ) {
                case kind::type_value::e_function:
                {
                    // funcion can be overloaded, so do not link with identifier
                    auto r_ty_d = type_detail_pool_->construct(
                        (type_id_t)type_id_nontype::e_function,
                        multiset_env
                        );
                    if ( is_captured && is_capture_enabled ) {
                        set_captured_value_info( identifier, found_env, r_ty_d );
                    }
                    return r_ty_d;
                }

                case kind::type_value::e_class:
                {
                    // class can NOT be overloaded, but can be specialized
                    auto const& ne = multiset_env->get_normal_environments();
                    if ( ne.size() == 1 ) {
                        // class defined normally
                        // class can not be overloaded, so only one symbol will exists in "multiset environment".
                        auto const& c_env
                            = cast_to<class_symbol_environment>( ne.at( 0 ) );

                        rill_dout << "()memoed.class " << c_env->get_qualified_name() << std::endl;

                        if ( c_env->is_incomplete() ) {
                            dispatch( c_env->get_related_ast(), c_env->get_parent_env() );
                        }

                        // link from given identifier!
                        c_env->connect_from_ast( identifier );

                        auto const& type_class_env = get_primitive_class_env( "type" );
                        if ( type_class_env->is_incomplete() ) {
                            dispatch(
                                type_class_env->get_related_ast(),
                                type_class_env->get_parent_env()
                                );
                        }
                        auto const& type_type_id
                            = g_env_->make_type_id( type_class_env, attribute::make_default_type_attributes() );

                        auto r_ty_d = type_detail_pool_->construct(
                            type_type_id,
                            type_class_env
                            );
                        if ( is_captured && is_capture_enabled ) {
                            set_captured_value_info( identifier, found_env, r_ty_d );
                        }
                        return r_ty_d;

                    } else {
                        // defined as class template
                        auto const& te = multiset_env->get_template_environments();
                        assert( te.size() != 0 );

                        auto r_ty_d = type_detail_pool_->construct(
                            (type_id_t)type_id_nontype::e_template_class,
                            multiset_env,
                            nullptr/*not nested*/,
                            std::make_shared<type_detail::template_arg_type>()
                            );
                        if ( is_captured && is_capture_enabled ) {
                            set_captured_value_info( identifier, found_env, r_ty_d );
                        }
                        return r_ty_d;
                    }
                }

                default:
                    std::cerr << "inner kind: " << debug_string( multiset_env->get_representation_kind() ) << std::endl;
                    assert( false && "[[CE]] invalid..." );
                    break;
                }
                break;

                assert( false );
                return nullptr;
            }


            case kind::type_value::e_variable:
            {
                auto const& v_env
                    = cast_to<variable_symbol_environment>( found_env );

                // memoize
                rill_dout << "() memoed.variable" << std::endl;
                v_env->connect_from_ast( identifier );

                // in class variable is forward referenceable
                if ( v_env->is_in_class() ) {
                    if ( v_env->is_incomplete() ) {
                        dispatch( v_env->get_related_ast(), v_env->get_parent_env() );
                    }
                }
                assert( v_env->get_type_id() != type_id_undefined );

                auto r_ty_d = type_detail_pool_->construct(
                    v_env->get_type_id(),
                    v_env
                    );
                 if ( is_captured && is_capture_enabled ) {
                     set_captured_value_info( identifier, found_env, r_ty_d );
                 }
                 return r_ty_d;
            }


            default:
                std::cerr << "kind: " << debug_string( found_env->get_symbol_kind() ) << std::endl;
                assert( false && "[[CE]] invalid..." );
                break;
            }

            assert( false );
            return type_detail_pool_->construct(
                type_id_undefined,
                nullptr
                );
        }


        auto analyzer::set_captured_value_info(
            ast::const_identifier_value_base_ptr const& identifier,
            environment_base_ptr const& found_env,
            type_detail_ptr const& ty_detail
            )
            -> void
        {
            //assert( block_envs_.top() != nullptr );
            //block_envs_.top()->propagate_outer_referenced_ast( identifier );
            assert( found_env->has_parent() );
            found_env->get_parent_env()->propagate_outer_referenced_ast( identifier );

            captured_type_details_.emplace( identifier->get_id(), ty_detail );

            rill_dout << "### OUTER: " << found_env->get_parent_env() << std::endl;
        }


        auto analyzer::import_module(
            ast::import_decl_unit const& decl,
            environment_base_ptr const& parent_env
            )
            -> void
        {
            auto const loaded_env = load_module( decl );
            if ( loaded_env == nullptr ) {
                semantic_error(
                    message_code::e_module_not_found,
                    nullptr,
                    format( "Failed to load module '%1%'" ) % decl.name,
                    true
                    );
            }

            parent_env->import_from( loaded_env );
        }

        auto analyzer::search_module(
            ast::import_decl_unit const& decl
            ) const
            -> boost::optional<std::tuple<fs::path, fs::path>>
        {
            // TODO: support structured module path
            auto const& load_filepath = fs::path( decl.name + ".rill" );

            // search from system libraries path
            for( auto&& lib_path : system_import_path_ ) {
                auto path = lib_path/load_filepath;
                if ( fs::exists( path ) ) {
                    return std::make_tuple( lib_path, std::move( path ) );
                }
            }

            // search from working dir
            auto path = working_dirs_.top()/load_filepath;
            if ( fs::exists( path ) ) {
                return std::make_tuple( working_dirs_.top(), std::move( path ) );
            }

            // -- Error
            // set extra error info
            save_stock_message_pivot();

            save_appendix_information(
                message_code::e_file_not_found,
                nullptr,
                format( "not found" )
                );

            return boost::none;
        }

        auto analyzer::load_module(
            ast::import_decl_unit const& decl
            )
            -> environment_base_ptr
        {
            auto const& found_path_set = search_module( decl );
            if ( found_path_set == boost::none ) {
                // search_module should set extra info
                return nullptr;
            }
            auto const& import_base = std::get<0>( *found_path_set );
            auto const& found_path = std::get<1>( *found_path_set );

            assert( found_path.is_absolute() );
            auto const mod_it = path_mod_rel_.find( found_path );
            if ( mod_it != path_mod_rel_.end() ) {
                // return cached env
                return mod_it->second;
            }

            // 1. parse
            auto const module_ast
                = syntax_analysis::parse( found_path );
            if ( module_ast == nullptr ) {
                save_stock_message_pivot();

                save_appendix_information(
                    message_code::e_failed_to_parse_module,
                    nullptr,
                    format( "Failed to parse" )
                    );

                return nullptr;
            }

            // 2. set import base path
            import_bases_.push( import_base );

            // 3. dispatch
            dispatch( module_ast );
            if ( get_report()->is_errored() ) {
                import_bases_.pop();

                save_stock_message_pivot();

                return nullptr;
            }

            // guarantee: there is new module env on top of stack after dispatching
            auto new_module_env = module_envs_.top();
            module_envs_.pop();

            //
            path_mod_rel_[found_path] = new_module_env; // cache

            //
            import_bases_.pop();
            working_dirs_.pop();

            return new_module_env;
        }


        auto analyzer::make_function_parameters_type_details(
            function_symbol_environment_ptr const& f_env,
            ast::function_definition_statement_base_ptr const& s
            )
            -> std::vector<type_detail_ptr>
        {
            std::vector<type_detail_ptr> tx;

            if ( s->is_class_function() ) {
                // TODO: fix
                bool const is_constructor
                    = s->get_identifier()->get_inner_symbol()->to_native_string() == "ctor";

                // calculate for "this" argument
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

                assert( f_env->is_in_class() );
                auto const& c_env
                    = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                        f_env->get_parent_class_env_id()
                        );
                assert( c_env != nullptr );

                // as this
                tx.emplace_back(
                    type_detail_pool_->construct(
                        g_env_->make_type_id( c_env, this_object_attr ),
                        nullptr,    // unused
                        nullptr,    // unused
                        nullptr,    // unused
                        false,       // xvalue
                        type_detail::evaluate_mode::k_everytime // TODO: fix
                        )
                    );
            }

            // make function parameter variable decl
            for( auto const& e : s->get_parameter_list() ) {
                assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                if ( e.decl_unit.init_unit.type ) {
                    // parameter variavle type is specified
                    resolve_type(
                        e.decl_unit.init_unit.type,
                        e.quality,
                        f_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            )
                        {
                            tx.emplace_back( ty_d );
                        });

                } else {
                    // type inferenced by result of evaluated [[default initializer expression]]

                    // TODO: implement type inference
                    rill_ice( "not implemented" );
                }
            }

            return tx;
        }

        auto analyzer::declare_function_parameters_from_list(
            function_symbol_environment_ptr const& f_env,
            ast::function_definition_statement_base_ptr const& s,
            std::vector<type_detail_ptr> const& param_types
            )
            -> void
        {
            rill_dregion {
                if ( s->is_class_function() ) {
                    assert( param_types.size() >= 1 );
                    assert( param_types.size() - 1 == s->get_parameter_list().size() );
                } else {
                    assert( param_types.size() == s->get_parameter_list().size() );
                }
            }

            auto const& params = s->get_parameter_list();

            for( std::size_t i=0, pi=0; i<param_types.size(); ++i ) {
                auto const& ty_id = param_types.at( i )->type_id;

                if ( i == 0 && s->is_class_function() ) {
                    // special treatment for "this"
                    construct_parameter_variable(
                        f_env,
                        ast::make_identifier( "this" ),
                        ty_id
                        );

                    continue;
                }

                auto const& e = params[pi];

                regard_variable_is_not_already_defined(
                    f_env,
                    e.decl_unit.name
                    );
                construct_parameter_variable(
                    f_env,
                    e.decl_unit.name,
                    ty_id
                    );

                ++pi;
            }
        }

        auto analyzer::declare_function_parameters(
            function_symbol_environment_ptr const& f_env,
            ast::function_definition_statement_base_ptr const& s
            )
            -> void
        {
            auto const& param_tyx = make_function_parameters_type_details(
                f_env,
                s
                );

            declare_function_parameters_from_list( f_env, s, param_tyx );
        }


        template<typename Deteals>
        inline auto check_is_args_valid(
            Deteals const& tyds
            )
            -> void
        {
            // TODO: check type_id_special
            assert(
                std::count_if(
                    tyds.cbegin(),
                    tyds.cend(), []( type_detail_ptr const& t ) {
                        return t->type_id == type_id_undefined
                            || is_nontype_id( t->type_id );
                    } ) == 0
                );
        }

        auto analyzer::evaluate_invocation_args(
            std::initializer_list<std::reference_wrapper<ast::expression_ptr>>&& exprs,
            environment_base_ptr const& parent_env
            )
            -> std::vector<type_detail_ptr>
        {
            std::vector<type_detail_ptr> argument_type_details;

            // make argument type list
            for( auto& val_expr : exprs ) {
                argument_type_details.push_back(
                    evaluate_invocation_arg( val_expr.get(), parent_env )
                    );
            }

            check_is_args_valid( argument_type_details );

            return argument_type_details;
        }

        auto analyzer::evaluate_invocation_args(
            type_detail_ptr const& reciever_ty_d,
            ast::expression_list& exprs,
            environment_base_ptr const& parent_env
            )
            ->std::vector<type_detail_ptr>
        {
            std::vector<type_detail_ptr> argument_type_details;

            if ( reciever_ty_d ) {
                if ( reciever_ty_d->nest ) {
                    // if lhs was nested && variable, add argument as "this"
                    //   Ex (1+3).operator+(6) should be callable.
                    //   In this case,
                    //       "reciever_ty_d" is "operator+"
                    //       "reciever_ty_d->nest->back()" is result of (1+3)
                    auto const& last_elem = reciever_ty_d->nest->back();

                    if ( is_type_id( last_elem->type_id ) ) {
                        // nested value has valid type! able to be this OR first arg
                        argument_type_details.push_back( last_elem );
                    }
                }
            }

            // make argument type list
            for( auto& val_expr : exprs ) {
                argument_type_details.push_back(
                    evaluate_invocation_arg( val_expr, parent_env )
                    );
            }

            check_is_args_valid( argument_type_details );

            return argument_type_details;
        }

        auto analyzer::evaluate_invocation_arg(
            ast::expression_ptr& expr,
            environment_base_ptr const& parent_env
            )
            -> type_detail_ptr
        {
            auto val_ty_d = dispatch( expr, parent_env );
            if ( val_ty_d->eval_mode == type_detail::evaluate_mode::k_only_compiletime ) {
                // replace expr
                substitute_by_ctfed_node( expr, val_ty_d, parent_env );
            }

            return val_ty_d;
        }


        auto analyzer::call_suitable_operator(
            ast::identifier_value_ptr const& op_name,
            ast::expression_ptr const& e,
            std::vector<type_detail_ptr> const& argument_type_details,
            environment_base_ptr const& parent_env,
            bool const do_universal_search
            )
            -> type_detail_ptr
        {
            rill_dout << "select member" << std::endl;
            assert( argument_type_details.size() >= 1 && "an argument for 'this' must be placed at(0)" );
            // solve
            auto callee_function_type_detail
                = select_member_element(
                    op_name,
                    argument_type_details.at(0),
                    parent_env,
                    do_universal_search
                    );
            if ( callee_function_type_detail == nullptr ) {
                return nullptr;
            }

            rill_dout << "call function" << std::endl;
            //
            if ( is_nontype_id( callee_function_type_detail->type_id ) ) {
                // reciever must be function
                if ( callee_function_type_detail->type_id == (type_id_t)type_id_nontype::e_function ) {
                    // call!
                    return call_function(
                        callee_function_type_detail,
                        argument_type_details,
                        e,
                        parent_env
                        );

                } else {
                    assert( false && "[Error]" );
                }

            } else {
                assert( false && "[Error]" );
            }

            return nullptr;
        }


        auto analyzer::call_constructor(
            ast::call_expression_ptr const& e,
            std::vector<type_detail_ptr> const& argument_type_details,
            environment_base_ptr const& parent_env
            )
            -> type_detail_ptr
        {
            type_detail_ptr b_ty_d = nullptr;

            resolve_type(
                ast::helper::make_id_expression( e->reciever_ ),
                attribute::holder_kind::k_val,     // TODO: fix
                parent_env,
                [&]( type_detail_ptr const& return_ty_d,
                     type const& ty,
                     class_symbol_environment_ptr const& class_env
                    ) {
                    rill_dout << "CONSTRUCTING type >> " << class_env->get_base_name() << std::endl;

                    // find constructor
                    auto const& multiset_env
                        = cast_to<multiple_set_environment>( class_env->find_on_env( "ctor" ) );
                    if ( multiset_env == nullptr ) {
                        rill_ice( "There are no ctor in class" );
                    }

                    assert( multiset_env->get_representation_kind() == kind::type_value::e_function );

                    // add implicit this parameter
                    std::vector<type_detail_ptr> argument_type_details_with_this(
                        argument_type_details.size() + 1
                        );
                    argument_type_details_with_this[0]
                        = type_detail_factory_->change_attributes(
                            return_ty_d,
                            attribute::modifiability_kind::k_mutable
                            );
                    std::copy(
                        argument_type_details.cbegin(),
                        argument_type_details.cend(),
                        argument_type_details_with_this.begin() + 1
                        );

                    auto const& function_env
                        = solve_function_overload(
                            multiset_env,                   // overload set
                            argument_type_details_with_this,// type detailes of arguments
                            nullptr,                        // template arguments
                            e,
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
                        std::make_shared<ast::evaluated_type_expression>(
                            return_ty_d->type_id
                            )
                        );

                    substituted_ast->line = e->reciever_->line;
                    substituted_ast->column = e->reciever_->column;

                    e->reciever_.swap( substituted_ast );
                });

            assert( b_ty_d != nullptr );
            return bind_type( e, b_ty_d );
        }


        auto analyzer::select_member_element(
            ast::identifier_value_base_ptr id,
            type_detail_ptr const& reciever_type_detail,
            environment_base_ptr const& parent_env,
            bool const do_universal_search
            )
            -> type_detail_ptr
        {
            auto const& reciever_type
                = g_env_->get_type_at( reciever_type_detail->type_id );
            assert( reciever_type.class_env_id != environment_id_undefined );

            auto const& reciever_class_env
                = g_env_->get_env_at_as_strong_ref( reciever_type.class_env_id );
            assert( reciever_class_env != nullptr );

            // 1. resolve identifier only from reciever_class_env
            auto const& selector_id_type_detail
                = solve_identifier( id, reciever_class_env, false );

            if ( selector_id_type_detail != nullptr ) {
                // found!
                // these elements are nested!

                // nest data
                auto const& nested
                    = reciever_type_detail->nest != nullptr
                    ? reciever_type_detail->nest
                    : std::make_shared<type_detail::nest_type>()
                    ;
                assert( nested != nullptr );

                // chain type_detail data
                // <- old [first evaled reciever type,
                //    second evaled...,
                //    ...,
                //    last evaled reciever type] -> new
                nested->push_back( reciever_type_detail );

                //
                if ( is_type_id( selector_id_type_detail->type_id ) ) {
                    // has valid type id, so found type is "variable"
                    if ( selector_id_type_detail->type_id != reciever_type_detail->type_id ) {
                        // parent's attributes are different from child.
                        //    so, delegate it from parent
                        auto const& selector_type
                            = g_env_->get_type_at( selector_id_type_detail->type_id );
                        assert( selector_type.class_env_id != environment_id_undefined );

                        auto const new_attr
                            = delegate_parent_attributes(
                                reciever_type.attributes,
                                selector_type.attributes
                                );
                        auto const new_type_id
                            = g_env_->make_type_id(
                                selector_type.class_env_id,
                                new_attr
                                );

                        return type_detail_pool_->construct(
                            new_type_id,
                            selector_id_type_detail->target_env,
                            nested,
                            selector_id_type_detail->template_args,
                            false,  // not xvalue, because variable has "name"
                            reciever_type_detail->eval_mode
                            );
                    }
                }

                // delegate parent attributes as they are
                return type_detail_pool_->construct(
                    selector_id_type_detail->type_id,
                    selector_id_type_detail->target_env,
                    nested,
                    selector_id_type_detail->template_args,
                    reciever_type_detail->is_xvalue,
                    reciever_type_detail->eval_mode
                    );

            } else {
                // NOT FOUND!!
                // 1. TODO: check existence of the function such as opDispatch

                // 2. Universal search
                if ( do_universal_search ) {
                    // search with excluding "class" env type
                    auto const& selector_id_type_detail
                        = solve_identifier(
                            id,
                            parent_env,
                            true,
                            kind::type_value::e_class
                            );

                    if ( selector_id_type_detail != nullptr ) {
                        // create new nest data
                        auto const& nested
                            = std::make_shared<type_detail::nest_type>();
                        assert( nested != nullptr );
                        nested->push_back( reciever_type_detail );

                        return type_detail_pool_->construct(
                            selector_id_type_detail->type_id,
                            selector_id_type_detail->target_env,
                            nested,
                            selector_id_type_detail->template_args,
                            reciever_type_detail->is_xvalue,
                            reciever_type_detail->eval_mode
                            );
                    }
                }
            }

            return nullptr;
        }


        auto analyzer::call_function(
            type_detail_ptr const& f_type_detail,
            std::vector<type_detail_ptr> const& argument_type_details,
            ast::expression_ptr const& e,
            environment_base_ptr const& parent_env
            )
            -> type_detail_ptr
        {
            assert( is_nontype_id( f_type_detail->type_id ) );
            rill_dout << "-> "
                      << debug_string( f_type_detail->target_env->get_symbol_kind() )
                      << std::endl;

            auto const& multiset_env
                = cast_to<multiple_set_environment>( f_type_detail->target_env );
            assert( multiset_env != nullptr );

            if ( multiset_env->get_representation_kind() != kind::type_value::e_function ) {
                // symbol type was not matched
                assert( false );
            }

            auto const& function_env
                = solve_function_overload(
                    multiset_env,
                    argument_type_details,          // type detailes of arguments
                    f_type_detail->template_args,   // template arguments
                    e,
                    parent_env
                    );
            assert( function_env != nullptr );

            // memoize called function env
            rill_dout << "memoed template" << std::endl;
            function_env->connect_from_ast( e );

            bool const is_xvalue = [&]() {
                auto const& tid = function_env->get_return_type_id();
                auto const& ty = g_env_->get_type_at( tid );
                return ty.attributes.quality == attribute::holder_kind::k_val;
            }();
            auto const eval_mode = [&]() {
                if ( function_env->has_attribute( attribute::decl::k_onlymeta ) ) {
                    return type_detail::evaluate_mode::k_only_compiletime;
                } else {
                    return type_detail::evaluate_mode::k_everytime;
                }
            }();

            return bind_type(
                e,
                type_detail_pool_->construct(
                    function_env->get_return_type_id(),
                    function_env,
                    nullptr,    // nullptr
                    nullptr,    // nullptr
                    is_xvalue,
                    eval_mode
                    )
                );
        }


        auto analyzer::get_filepath(
            ast::const_ast_base_ptr const& ast
            ) const
            -> boost::filesystem::path
        {
            auto const& rel_env
                = ast != nullptr
                ? g_env_->get_related_env_by_ast_ptr( ast )
                : nullptr;

            if ( rel_env != nullptr ) {
                auto const& root
                    = rel_env->root_env();
                assert( root != nullptr );
                auto const& module_env
                    = cast_to<module_environment const>( root );

                return module_env->get_filepath();

            } else {
                auto const& module_ast
                    = std::static_pointer_cast<ast::module>(
                        g_env_->get_related_ast( module_envs_.top()->get_id() )
                        );

                return module_ast->fullpath;
            }
        }


        auto analyzer::regard_variable_is_not_already_defined(
            const_environment_base_ptr const& top,
            ast::const_identifier_value_base_ptr const& id
            )
            -> void
        {
            if ( auto const& v = top->find_on_env( id ) ) {
                save_stock_message_pivot();

                auto const& val_decl_ast
                    = g_env_->get_related_ast( v->get_id() );

                save_appendix_information(
                    message_code::e_reference,
                    val_decl_ast,
                    format( "reference" )
                    );

                semantic_error(
                    message_code::e_variable_is_already_defined,
                    id,
                    format( "variable is already defined" ),
                    true
                    );
            }
        }

        auto analyzer::make_pointer_type(
            type_id_t const& type_id,
            ast::ast_base_ptr const& connected_node,
            environment_base_ptr const& parent_env
            )
            -> type_detail_ptr
        {
            auto ty = g_env_->get_type_at( type_id ); // make copy
            attribute::detail::set_type_attribute( ty.attributes, attribute::holder_kind::k_val );
            auto const& element_type_id = g_env_->make_type_id(
                ty.class_env_id,
                ty.attributes
                );

            auto const& inner_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    ty.class_env_id
                    );

            ast::expression_list args = {
                std::make_shared<ast::evaluated_type_expression>( element_type_id )
            };

            auto const& instance
                = ast::helper::make_id_expression(
                    std::make_shared<ast::term_expression>(
                        std::make_shared<ast::template_instance_value>(
                            "ptr", std::move( args ), true
                            )
                        )
                    );

            return resolve_type(
                instance,
                attribute::holder_kind::k_val,
                parent_env->root_env(),
                [&]( type_detail_ptr const& ty_d,
                     type const& ty,
                     class_symbol_environment_ptr const& class_env
                    ) {
                    assert( class_env->is_pointer() );

                    class_env->connect_from_ast( connected_node );

                    // propagate
                    assert( ty.attributes.modifiability != attribute::modifiability_kind::k_none );
                    if ( ty.attributes.modifiability != attribute::modifiability_kind::k_immutable ) {
                        if ( !inner_c_env->is_immutable() ) {
                            class_env->set_traits_flag( class_traits_kind::k_has_non_immutable_alias, true );
                        }
                    }
                } );
        }


        auto analyzer::construct_parameter_variable(
            function_symbol_environment_ptr const& f_env,
            ast::const_identifier_value_base_ptr const& variable_name,
            type_id_t const& type_id
            )
            -> void
        {
            auto const& t = g_env_->get_type_at( type_id );

            // construct variable
            // NOTE: do not link with ast
            auto v_env = f_env->incomplete_construct(
                kind::k_variable,
                variable_name
                );

            // complete variable
            v_env->complete( type_id );

            f_env->append_parameter_variable( v_env );
        }

    } // namespace semantic_analysis
} // namespace rill
