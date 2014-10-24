//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/semantic_analysis/analyzer/identifier_solver.hpp>

#include <rill/environment/environment.hpp>

#include <rill/utility/colorize.hpp>
#include <rill/utility/tie.hpp>

#include <unordered_map>
#include <cmath>


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


        class analyzer::builtin_class_envs_cache
        {
            friend analyzer;

        public:
            builtin_class_envs_cache( environment_base_ptr const& root_env )
            {
                auto install_primitive_class
                    = [&]( std::string const& type_name ) mutable
                    {
                        primitive_cache_[type_name]
                            = to_unique_class_env(
                                root_env->lookup( type_name )
                                );
                    };

                install_primitive_class( "type" );
                install_primitive_class( "int" );
                install_primitive_class( "string" );
            }

        private:
            inline auto find_primitive( std::string const& type_name ) const
                -> const_class_symbol_environment_ptr
            {
                auto const it = primitive_cache_.find( type_name );
                assert( it != primitive_cache_.cend() );

                return it->second;
            }

        private:
            std::unordered_map<std::string, const_class_symbol_environment_ptr> primitive_cache_;
        };


        //
        analyzer::analyzer(
            environment_base_ptr const& root_env,
            intrinsic_function_action_holder_ptr const& holder
            )
            : root_env_( root_env )
            , type_detail_pool_( std::make_shared<type_detail_pool_t>() )
            , ctfe_engine_( compile_time::llvm_engine::make_ctfe_engine( this, root_env, holder, type_detail_pool_ ) )
            , builtin_class_envs_cache_( std::make_shared<builtin_class_envs_cache>( root_env ) )
        {
            type_detail_factory_
                = std::make_shared<type_detail_factory>( root_env_, type_detail_pool_ );
        }

        //
        auto analyzer::get_primitive_class_env( std::string const& type_name ) const
            -> const_class_symbol_environment_ptr
        {
            return builtin_class_envs_cache_->find_primitive( type_name );
        }

        //
        auto analyzer::ref_type(
            type_detail_ptr const& ty_detail
            ) const -> type const&
        {
            return root_env_->get_type_at( ty_detail->type_id );
        }

        auto make_mangled_name(
            const_class_symbol_environment_ptr const& c_env,
            attribute::type_attributes const& attr
            )
            -> std::string
        {
            assert( c_env != nullptr );
            //assert( c_env->is_checked() );

            std::string s;
            s += std::to_string( c_env->get_mangled_name().size() );
            s += c_env->get_mangled_name();

            s += [&]() {
                switch( attr.quality )
                {
                case attribute::holder_kind::k_val:
                    return "VAL";
                case attribute::holder_kind::k_ref:
                    return "REF";
                default:
                    assert( false );
                }
            }();

            s += [&]() {
                switch( attr.modifiability )
                {
                case attribute::modifiability_kind::k_mutable:
                    return "MUT";
                case attribute::modifiability_kind::k_const:
                    return "CST";
                case attribute::modifiability_kind::k_immutable:
                    return "IMM";
                default:
                    assert( false );
                }
            }();

            return s;
        }


        auto make_mangled_name( const_function_symbol_environment_ptr const& f_env )
            -> std::string
        {
            assert( f_env != nullptr );
            assert( f_env->is_checked() );

            std::string s;

            s += f_env->get_base_name();

            for( auto const& type_id : f_env->get_parameter_type_ids() ) {
                auto const& param_type = f_env->get_type_at( type_id );

                s += make_mangled_name(
                    f_env->get_env_at_as_strong_ref<class_symbol_environment const>(
                        param_type.class_env_id
                        ),
                    param_type.attributes
                    );
            }

            return s;
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
                = root_env_->make_type_id(
                    root_env_->get_env_at_as_strong_ref<class_symbol_environment>( t.class_env_id ),
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

            std::cout << "TEMPLATE param size is " << template_parameters.size() << std::endl;

            for( std::size_t i=0; i<template_parameters.size(); ++i ) {
                auto const& template_parameter = template_parameters.at( i );

                //
                // 1. declare template parameters
                //
                assert(
                    template_parameter.decl_unit.init_unit.type != nullptr
                    || template_parameter.decl_unit.init_unit.initializer != nullptr
                    );

                if ( template_parameter.decl_unit.init_unit.type ) {
                    resolve_type(
                        template_parameter.decl_unit.init_unit.type,
                        template_parameter.quality,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             const_class_symbol_environment_ptr const& class_env
                            )
                        {
                            std::cout << "Construct template parameter val / index : " << i << std::endl;

                            // declare the template parameter into function env as variable
                            auto const& v_env
                                = inner_env->construct(
                                    kind::k_variable,
                                    template_parameter.decl_unit.name,
                                    nullptr/*TODO: change to valid ptr to ast*/,
                                    class_env,
                                    ty.attributes
                                    );

                            declared_envs[i] = v_env;
                        });

                } else {
                    // TODO: set as TYPE
                    assert( false && "TODO: it will be type" );
                }
            }

            return declared_envs;
        }


        // declare template parameter variable, and substitute explicit argument
        auto analyzer::tp(
            ast::parameter_list const& template_parameter_list,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& inner_env,
            environment_base_ptr const& parent_env
            )
            -> void
        {
            // template parameters
            // import template parameter's variables with instantiation!

            std::cout << "TEMPLATE param size is " << template_parameter_list.size() << std::endl;

            // 1. declare template parameters
            auto decl_arg_holder
                = declare_template_parameter_variables(
                    template_parameter_list,
                    inner_env,
                    parent_env
                    );


            for( std::size_t i=0; i<template_parameter_list.size(); ++i ) {
                auto const& template_parameter = template_parameter_list.at( i );



                // TODO: add error check to varidate param and arg size...

                std::cout << "pp i = " << i << " / " << template_args->size() << std::endl;

                //
                // 2. substitute template arguments
                // save argument value, if the template argument was passed explicitly,
                // if NOT, it will be deduced after that...
                if ( i < template_args->size() ) {
                    // do process, if argument was given

                    std::cout << "TEMPLATE ARGS!! " << i << std::endl;
                    auto const& template_var_env = decl_arg_holder[i];
                    auto const& template_arg = template_args->at( i );

                    // DEBUG
                    {
                        if ( template_arg.is_type() ) {
                            auto const& t_detail
                                = static_cast<type_detail_ptr>( template_arg.element );
                            assert( t_detail != nullptr );

                            // get environment of arguments
                            auto const& tt
                                = root_env_->get_type_at( t_detail->type_id );

                            auto const& c_e
                                = std::static_pointer_cast<class_symbol_environment const>(
                                    root_env_->get_env_strong_at(
                                        tt.class_env_id
                                        )
                                    );

                            std::cout << "** typeid << " << t_detail->type_id << std::endl
                                      << "** " << tt.class_env_id << std::endl;

                            std::cout << "BINDED " << template_var_env->get_id()
                                      << " -> " << c_e->get_qualified_name() << std::endl;
                        } else {
                            std::cout << "value" << std::endl;
                        }
                    }

                    // TODO: fix...
                    //ctfe_engine_->value_holder()->bind_value(
                    //    template_var_env->get_id(),
                    //    template_arg
                    //    );

                }
            } // for
        }



        auto analyzer::instanciate_class_candidate(
            type_detail_ptr const& target_ty_detail,
            environment_base_ptr const& parent_env
            )
            -> std::vector<class_symbol_environment_ptr>
        {
            assert( target_ty_detail->type_id == (type_id_t)type_id_nontype::e_template_class );

            //
            std::vector<class_symbol_environment_ptr> candidates;
#if 0
            std::cout << "template class! : Arg num~ "
                      << target_ty_detail->template_args->size()
                      << std::endl;

            // take a template set
            auto const& template_set_env
                = target_ty_detail->target_env->cast_to<multiple_set_environment>();
            assert( template_set_env != nullptr );


            auto const& template_args
                = target_ty_detail->template_args;

            //
            for( auto const& template_env : template_set_env->get_candidates() ) {
                // TODO: add template length check...
                // TODO: remove environment when instantiation is failed.

                std::cout << "hogehoge !" << std::endl;

                // if number of template arguments is over, skip
                if ( template_args->size() > template_env->get_parameter_num() )
                    continue;

                // TODO: type check(even if template contains "class")

                //
                auto const& template_ast
                    = std::static_pointer_cast<ast::template_statement>(
                        template_env->get_related_ast()
                        );
                assert( template_ast != nullptr );

                std::cout << "class !" << std::endl;

                // ==================================================
                // INNER class
                // ==================================================

                // make new ast(cloned)
                auto const& class_ast
                    = std::static_pointer_cast<ast::class_definition_statement>(
                        template_ast->clone_inner_node()
                        );
                assert( class_ast != nullptr );

                std::cout << "class !" << std::endl;

                // create class env
                auto c_env
                    = template_set_env->allocate_env<class_symbol_environment>( template_set_env->get_id() );

                std::cout << "fugafuga" << std::endl;

                std::cout << "TEMPLATE bef" << std::endl;


                // ***********
                tp(
                    template_ast->get_parameter_list(),
                    template_args,
                    c_env,
                    parent_env
                    );


                std::cout << "TEMPLATE aftre" << std::endl;

                //
                // class instanciation
                if ( !complete_class(
                         class_ast,
                         c_env,
                         template_args
                         )
                    ) {
                    // Already completed...
                    // maybe, error...
                    assert( false );
                    continue;
                }

                // link
                c_env->link_with_ast( class_ast );
                std::cout << "TEMPLATE finished" << std::endl;

                // register this class environment to the parent environment
                std::static_pointer_cast<
                    single_identifier_environment_base
                    >( template_set_env->get_parent_env() )->insert( c_env );

                //
                candidates.push_back( c_env );
            }

#endif
            return candidates;
        }


        //
        // returns "type" type
        //
        // TODO: rename instantiate
        auto analyzer::instanciate_class(
            type_detail_ptr const& target_ty_detail,
            environment_base_ptr const& parent_env
            )
            -> class_symbol_environment_ptr
        {
            //
            auto const& c_env
                = instanciate_class(
                    target_ty_detail,
                    parent_env,
                    []( std::vector<class_symbol_environment_ptr> const & c_candidate_envs ) {
                        assert( c_candidate_envs.size() == 1 && "[ice]" );
                        // TODO: add duplication check
                        return c_candidate_envs[0];
                    } );

            // return type should be "type"
            // TODO: fix
            auto const& type_class_env = get_primitive_class_env( "type" );
            assert( type_class_env != nullptr );  // literal type must exist

            target_ty_detail->type_id
                = type_class_env->make_type_id( type_class_env, attribute::make_default_type_attributes() );

            target_ty_detail->target_env = nullptr; // unused

            return c_env;
        }



        auto analyzer::evaluate_template_args(
            ast::expression_list const& arguments,
            environment_base_ptr const& parent_env
            )
            -> type_detail::template_arg_type
        {
            std::cout << "eval template arguments!!!" << std::endl;

            // evaluate template arguments...
            type_detail::template_arg_type template_args;

            // TODO: implement
            for( auto const& expression : arguments ) {
                //
                std::cout << "template expresison!!!!!!" << std::endl;
                auto const& argument_ty_detail = dispatch( expression, parent_env );

                // get environment of arguments
                auto const& ty
                    = root_env_->get_type_at( argument_ty_detail->type_id );

                auto const& c_env
                    = std::static_pointer_cast<class_symbol_environment const>(
                        root_env_->get_env_strong_at( ty.class_env_id )
                        );
                assert( c_env != nullptr );

                // eval expression of arguments
                auto const& argument_evaled_value
                    = ctfe_engine_->execute_as_raw_storage( expression, parent_env );
                assert( argument_evaled_value != nullptr );

                //
                auto ta = [&]() -> type_detail::dependent_type {
                    // TODO: fix cond
                    if ( c_env->get_base_name() == "type" ) {
                        return {
                            argument_ty_detail,
                            static_cast<type_detail_ptr>( argument_evaled_value ),
                            dependent_value_kind::k_type
                        };
                    } if ( c_env->get_base_name() == "int" ) {
                        return {
                            argument_ty_detail,
                            argument_evaled_value,
                            dependent_value_kind::k_int32
                        };

                    } else {
                        assert( false && "[[ice]] value parameter was not supported yet" );
                        return { nullptr, nullptr, dependent_value_kind::k_none };
                    }
                }();
                std::cout << "argt: " << c_env->get_qualified_name() << std::endl;

                template_args.push_back( ta );
            }

            return template_args;
        }





        //
        auto qualifier_conversion_from_ref(
            attribute::type_attributes const& parameter_attributes,
            attribute::type_attributes const& argument_attributes
            )
            -> boost::optional<attribute::type_attributes>
        {
            auto result_attr = argument_attributes;

            switch( parameter_attributes.quality )  // to
            {
            case attribute::holder_kind::k_ref:
            {
                // ref to ref
                result_attr <<= attribute::holder_kind::k_ref;
                break;
            }

            case attribute::holder_kind::k_val:
            {
                // ref to val
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
            attribute::type_attributes const& argument_attributes
            )
            -> boost::optional<attribute::type_attributes>
        {
            auto result_attr = argument_attributes;

            switch( parameter_attributes.quality )  // to
            {
            case attribute::holder_kind::k_ref:
            {
                // val to ref
                switch( argument_attributes.modifiability ) {
                case attribute::modifiability_kind::k_immutable:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_const:
                        break;
                    case attribute::modifiability_kind::k_mutable:
                        return boost::none;
                    } // switch( parameter_attributes.attributes.modifiability )
                    break;

                case attribute::modifiability_kind::k_const:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                    case attribute::modifiability_kind::k_const:
                        break;
                    case attribute::modifiability_kind::k_mutable:
                        return boost::none;
                    } // switch( parameter_attributes.attributes.modifiability )
                    break;

                case attribute::modifiability_kind::k_mutable:
                    switch( parameter_attributes.modifiability ) {
                    case attribute::modifiability_kind::k_immutable:
                        return boost::none;
                    case attribute::modifiability_kind::k_const:
                    case attribute::modifiability_kind::k_mutable:
                        break;
                    } // switch( target_type.attributes.modifiability )
                    break;
                } // switch( parameter_attributes.modifiability )

                result_attr <<= attribute::holder_kind::k_ref;
                break;
            }

            case attribute::holder_kind::k_val:
            {
                // val to val
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
            attribute::type_attributes const& argument_attributes
            )
            -> boost::optional<attribute::type_attributes>
        {
            //
            switch( argument_attributes.quality ) // from
            {
            case attribute::holder_kind::k_ref:
                return qualifier_conversion_from_ref( parameter_attributes, argument_attributes );

            case attribute::holder_kind::k_val:
                return qualifier_conversion_from_val( parameter_attributes, argument_attributes );

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
            auto const& arg_type = root_env_->get_type_at( arg_type_detail->type_id );

            if ( param_type_detail->template_args == nullptr ) {
                // simple type match
                std::cout << " ? simple type match" << std::endl;
                auto const& param_type = root_env_->get_type_at( param_type_detail->type_id );

                if ( param_type.is_incomplete() ) {
                    // determine this type_detail has a pointer to the template param env
                    std::cout << " ? : template param" << std::endl;

                    auto const& r_tv_env = param_type_detail->target_env;
                    assert( r_tv_env != nullptr );
                    assert( r_tv_env->get_symbol_kind() == kind::type_value::e_variable );
                    auto const& template_var_env = std::static_pointer_cast<variable_symbol_environment>( r_tv_env );

                    auto ty_d = reinterpret_cast<type_detail*>(
                        ctfe_engine_->value_holder()->ref_value( template_var_env->get_id() )
                        );
                    assert( ty_d != nullptr );

                    auto const new_parameter_val_type_attr = arg_type.attributes - param_type.attributes;
                    auto const new_paramater_val_type_id
                        = root_env_->make_type_id( arg_type.class_env_id, new_parameter_val_type_attr );
                    std::cout << "new_parameter_val_type_attr: " << new_parameter_val_type_attr << std::endl;

                    // update value
                    ty_d->type_id = new_paramater_val_type_id;

                    // make "parameter"'s type
                    auto const new_parameter_type_attr
                        = mask_by( new_parameter_val_type_attr, param_type.attributes );
                    auto const new_paramater_type_id
                        = root_env_->make_type_id( arg_type.class_env_id, new_parameter_type_attr );
                    std::cout << "new_parameter_type_attr: " << new_parameter_type_attr << std::endl;

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
                auto const& param_type = root_env_->get_type_at( param_type_id );
                auto const& arg_type = root_env_->get_type_at( arg_type_id );

                if ( param_type.class_env_id == arg_type.class_env_id ) {
                    // same class, so check quarity conversion
                    if ( auto&& attribute_opt = qualifier_conversion( param_type.attributes, arg_type.attributes ) ) {
                        // qualifier conversion match
                        return std::make_tuple(
                            function_match_level::k_qualifier_conv_match,
                            nullptr
                            );

                    } else {
                        // unmatched
                        return std::make_tuple(
                            function_match_level::k_no_match,
                            nullptr
                            );
                    }

                } else {
                    // TODO: implement implicit conversion match(k_implicit_conv_match)
                    // currentry failed immediately
                    return std::make_tuple(
                        function_match_level::k_no_match,
                        nullptr
                        );
                }
            }
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
                std::cout << "[overloads] " << f_env->get_mangled_name() << " ... is_checked ? " << f_env->is_checked() << std::endl
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

                    std::cout << "Param Type Conv (" << i << "): ";

                    RILL_PP_TIE( level, conv_function_env,
                                 try_type_conversion(
                                     param_type_id,
                                     arg_type_id,
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

            std::cout << " !== overload ======================================================" << std::endl
                      << "best match: " << static_cast<int>( best_matched_level ) << std::endl;

            for( auto&& env : f_candidate_envs ) {
                std::cout << std::endl
                          << "  !!!!! condidate found >>> " << std::endl
                          << std::endl;
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
            multiple_set_environment_ptr const& set_env,
            std::vector<type_detail_ptr> const& arg_types,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env
            )
            -> void
        {
            // solve FUNCTION template
            for( auto&& env : set_env->get_template_environments() ) {
                std::cout << colorize::standard::fg::red
                      << "!!!! template: " << std::endl
                      << colorize::standard::reset
                      << std::endl;

                //
                auto const& template_ast
                    = std::static_pointer_cast<ast::template_statement>(
                        env->get_related_ast()
                        );
                assert( template_ast != nullptr );

                //
                auto const& function_def_ast
                    = std::static_pointer_cast<ast::function_definition_statement>(
                        template_ast->clone_inner_node()
                        );
                assert( function_def_ast != nullptr );
                assert( function_def_ast->get_identifier() != nullptr );

                // construct incomplete function emvironment frame
                auto instanting_f_env
                    = set_env->allocate_inner_env<function_symbol_environment>(
                        function_def_ast->get_identifier()->get_inner_symbol()->to_native_string()
                        );

                //
                std::vector<int> solved_template_params_flag( template_ast->parameter_list_.size() );

                // declare template parameters as variables
                auto const decl_template_var_envs
                    = declare_template_parameter_variables(
                        template_ast->parameter_list_,
                        instanting_f_env,
                        env
                        );

                if ( template_args != nullptr ) {
                    // template args are provided

                    // compare template's arguments and parameters
                    // TODO: add error process
                    assert( template_args->size() <= decl_template_var_envs.size() );

                    for( std::size_t i=0; i<template_args->size(); ++i ) {
                        auto const& template_parameter = template_ast->parameter_list_.at( i );

                        // substitute template arguments
                        // save template argument value to the template variables env
                        std::cout << "TEMPLATE ARGS index: " << i << std::endl;
                        auto const& template_var_env = decl_template_var_envs.at( i );
                        auto const& template_arg = template_args->at( i );

                        // DEBUG
                        {
                            if ( template_arg.is_type() ) {
                                auto const& t_detail
                                    = static_cast<type_detail_ptr>( template_arg.element );
                                assert( t_detail != nullptr );

                                // get environment of arguments
                                auto const& tt
                                    = root_env_->get_type_at( t_detail->type_id );

                                auto const& c_e
                                    = std::static_pointer_cast<class_symbol_environment const>(
                                        root_env_->get_env_strong_at(
                                            tt.class_env_id
                                            )
                                        );

                                std::cout << "** typeid << " << t_detail->type_id << std::endl
                                          << "** " << tt.class_env_id << std::endl;

                                std::cout << "BINDED " << template_var_env->get_id()
                                          << " -> " << c_e->get_qualified_name() << std::endl;
                            } else {
                                std::cout << "value" << std::endl;
                            }
                        }

                        // TODO: fix...
                        //ctfe_engine_->value_holder()->bind_value(
                        //    template_var_env->get_id(),
                        //    template_arg
                        //    );
                    } // for
                } // if

                auto const arg_index_until_provided
                    = template_args != nullptr
                    ? template_args->size()
                    : 0;
                for( std::size_t i=arg_index_until_provided; i<decl_template_var_envs.size(); ++i ) {
                    // assign empty type value
                    std::cout << "= Assign empty type value / index : " << i << std::endl;

                    auto const& template_var_env = decl_template_var_envs.at( i );

                    ctfe_engine_->value_holder()->bind_value(
                        template_var_env->get_id(),
                        type_detail_pool_->construct(
                            root_env_->make_type_id(),  // empty type
                            template_var_env            // link to template env
                            )
                        );
                }

                std::vector<type_detail_ptr> presetted_param_types;

                // resolve types of parameters
                // make function parameter variable decl
                for( auto const& e : function_def_ast->get_parameter_list() ) {
                    assert( e.decl_unit.init_unit.type != nullptr || e.decl_unit.init_unit.initializer != nullptr );

                    if ( e.decl_unit.init_unit.type ) { // is parameter variavle type specified ?
                        resolve_type(
                            e.decl_unit.init_unit.type,
                            e.quality,
                            instanting_f_env,
                            [&]( type_detail_ptr const& ty_d,
                                 type const& ty,
                                 const_class_symbol_environment_ptr const& class_env
                                )
                            {
                                presetted_param_types.push_back( ty_d );
                            });

                    } else {
                        // type inferenced by result of evaluated [[default initializer expression]]

                        // TODO: implement type inference
                        assert( false );
                    }
                }

                // type decuce!
                assert( presetted_param_types.size() == arg_types.size() );
                for( std::size_t i=0; i<presetted_param_types.size(); ++i ) {
                    std::cout << "Template arg/param type inference... / index : " << i << std::endl;
                    auto& param_type = presetted_param_types[i];
                    auto const& arg_type = arg_types[i];

                    auto new_ty_d = infer_param_type_from_arg_type( param_type, arg_type );
                    std::cout << " == is_succeeded : " << ( new_ty_d != nullptr ) << std::endl;
                    if ( new_ty_d == nullptr ) {
                        // TODO: fail! skip this function instatiation
                        assert( false && "" );
                    }

                    // update
                    param_type = new_ty_d;
                }

                // TODO: check existance of incomplete template vaiables

                //
                for( std::size_t i=0; i<presetted_param_types.size(); ++i ) {
                    auto const& e = function_def_ast->get_parameter_list()[i];
                    auto const& ty_id = presetted_param_types[i]->type_id;

                    instanting_f_env->parameter_variable_construct( e.decl_unit.name, ty_id );
                }

                //
                instanting_f_env->change_progress_to_checked();

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
                instanting_f_env->complete( make_mangled_name( instanting_f_env ) );
                instanting_f_env->link_with_ast( function_def_ast );

                //
                set_env->add_to_instanced_environments( instanting_f_env );

                std::cout << colorize::standard::fg::red
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
            // return type check
            type_id_t current_ret_type_id
                = f_env->is_return_type_decided()
                ? f_env->get_return_type_id()
                : type_id_undefined;

            // DEBUG
            if ( f_env->is_return_type_decided() ) {
                auto const ty = root_env_->get_type_at( current_ret_type_id );
                auto const& c_env = root_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    ty.class_env_id
                    );
                std::cout << "RETURN TYPE => "
                          << make_mangled_name( c_env, ty.attributes )
                          << std::endl;
            }

            for( auto&& r_type_id : f_env->get_return_type_candidates() ) {
                auto const ty = root_env_->get_type_at( r_type_id );
                auto const& c_env = root_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    ty.class_env_id
                    );
                std::cout << "CANDIDATE: RETURN TYPE => "
                          << ty.attributes
                          << std::endl;

                if ( current_ret_type_id != type_id_undefined ) {
                    if ( r_type_id != current_ret_type_id ) {
                        assert( false && "[Error] return type is different from function signature" );
                    }

                } else {
                    current_ret_type_id = r_type_id;
                }
            }
            f_env->decide_return_type( current_ret_type_id );

            // Return type
            if ( !f_env->is_return_type_decided() ) {
                assert( false && "[Error] return type was not determined..." );
            }
        }

        // solve the function from overload set and returns one function environment
        auto analyzer::solve_function_overload(
            multiple_set_environment_ptr const& set_env,
            std::vector<type_detail_ptr> const& arg_types,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env
            )
            -> function_symbol_environment_ptr
        {
            //
            assert( set_env->get_representation_kind() == kind::type_value::e_function );

            std::cout << colorize::standard::fg::red
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
                    assert( false && "Error: Suitable function was not found" );
                    return nullptr;
                }

                if ( o_res_f_envs->size() != 1 ) {
                    assert( false && "Error: Callable functions were anbigous" );
                    return nullptr;
                }

                return o_res_f_envs->at( 0 );

            } else {
                // solve only templated functions
            }


            return nullptr;
        }


        // for Identifier
        auto analyzer::solve_identifier(
            ast::const_identifier_value_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_not_lookup
            ) -> type_detail_ptr
        {
            return generic_solve_identifier( identifier, parent_env, do_not_lookup );
        }


        // for Template Instance Identifier
        auto analyzer::solve_identifier(
            ast::const_template_instance_value_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_not_lookup
            ) -> type_detail_ptr
        {
            auto const ty_detail
                = generic_solve_identifier( identifier, parent_env, do_not_lookup );
            if ( ty_detail == nullptr ) {
                // propagate nullptr...
                return nullptr;
            }

            // v. template_argument()

            std::cout << "eval template arguments!!!" << std::endl;

            // evaluate template arguments
            type_detail::template_arg_type template_args
                = evaluate_template_args(
                    identifier->template_argument(),
                    parent_env
                    );

            // set evaluated template args
            assert( ty_detail->template_args != nullptr );
            (*ty_detail->template_args) = std::move( template_args );

            // class template instantiation!!!
            if ( is_nontype_id( ty_detail->type_id ) ) {
                if ( ty_detail->type_id == (type_id_t)type_id_nontype::e_template_class ) {
                    // returns instances class symbol environment
                    auto const i_c_env
                        = instanciate_class(
                            ty_detail,
                            parent_env
                            );

                    // !! important
                    // memoize
                    std::cout << "()memoed.template_class" << std::endl;
                    i_c_env->connect_from_ast( identifier );
                }
            }

            return ty_detail;
        }


        //
        auto analyzer::generic_solve_identifier(
            ast::const_identifier_value_base_ptr const& identifier,
            environment_base_ptr const& parent_env,
            bool const do_lookup
            ) -> type_detail_ptr
        {
            // TODO: check that identifier is from root

            auto const found_env
                = do_lookup
                ? parent_env->lookup( identifier )
                : parent_env->find_on_env( identifier )
                ;
            if ( found_env == nullptr ) {
                // identifier was not found, return nullptr
                return nullptr;
            }

            // debug
            std::cout << "## Finding Identifier: " << identifier->get_inner_symbol()->to_native_string() << std::endl
                      << "## astid: " << identifier->get_id() << std::endl
                      << "## kind: " << debug_string( found_env->get_symbol_kind() ) << std::endl
                      << (const_environment_base_ptr)parent_env << std::endl;


            switch( found_env->get_symbol_kind() ) {
            case kind::type_value::e_multi_set:
            {
                auto const& set_env
                    = std::static_pointer_cast<multiple_set_environment>( found_env );

                //
                switch( set_env->get_representation_kind() ) {
                case kind::type_value::e_function:
                {
                    // funcion can be oveloaded, so do not link with identifier
                    return type_detail_pool_->construct(
                        (type_id_t)type_id_nontype::e_function,
                        set_env
                        );
                }

                case kind::type_value::e_class:
                {
                    // Class identifier should be "type" type

                    // TODO: completion the incomplete class

                    // class can not be overloaded, so only one symbol will exist in "set environment".
                    auto const& ne = set_env->get_normal_environments();
                    assert( ne.size() == 1 );

                    auto const& class_env
                        = cast_to<class_symbol_environment>( ne.at( 0 ) );
                    assert( class_env != nullptr );

                    std::cout << "()memoed.class " << class_env->get_qualified_name() << std::endl;
                    // link with given identifier!
                    class_env->connect_from_ast( identifier );

                    // TODO: cache this values
                    auto const& type_class_env = [&](){
                        auto const& generic_type_class_set_env
                        = root_env_->lookup( ast::make_identifier( "type" ) );
                        assert( generic_type_class_set_env != nullptr );  // literal type must exist
                        auto const& type_class_set_env
                        = cast_to<multiple_set_environment>( generic_type_class_set_env );

                        assert( type_class_set_env != nullptr );

                        auto const& ne
                        = type_class_set_env->get_normal_environments();
                        assert( ne.size() == 1 );

                        return cast_to<class_symbol_environment>( ne.at( 0 ) );
                    }();
                    assert( type_class_env != nullptr );

                    auto const& type_type_id
                        = type_class_env->make_type_id( type_class_env, attribute::make_default_type_attributes() );

                    return type_detail_pool_->construct(
                        type_type_id,
                        type_class_env
                        );
                }

                default:
                    std::cerr << "kind: " << debug_string( set_env->get_representation_kind() ) << std::endl;
                    assert( false && "[[CE]] invalid..." );
                    break;
                }
                break;

                assert( false );
            }


            case kind::type_value::e_variable:
            {
                auto const& v_env
                    = std::static_pointer_cast<variable_symbol_environment>( found_env );

                // memoize
                std::cout << "() memoed.variable" << std::endl;
                v_env->connect_from_ast( identifier );

                // in class variable is forward referenceable
                if ( v_env->is_in_class() ) {
                    if ( v_env->is_incomplete() ) {
                        dispatch( v_env->get_related_ast(), v_env->get_parent_env() );
                    }
                }
                assert( v_env->get_type_id() != type_id_undefined );

                return type_detail_pool_->construct(
                    v_env->get_type_id(),
                    v_env
                    );
            }


#if 0
            case kind::type_value::e_template_set:
            {
                std::cout << "TEMPLATE SET" << std::endl;
                auto const& template_set_env
                    = std::static_pointer_cast<template_set_environment>( found_env );

                switch( template_set_env->get_inner_env_symbol_kind() ) {
                case kind::type_value::e_class:
                {
                    // DO NOT FORGET TO CONNECT to the instances type from ast


                    // Class identifier should be "type" type...?
                    // COUTION: type_class_env will be multiple_set
                    auto const& type_class_env = root_env_->lookup( ast::make_identifier( "type" ) );
                    assert( type_class_env != nullptr );  // literal type must exist

                    auto const& ty_id = type_class_env->make_type_id(
                        type_class_env,
                        determine_type_attributes()
                        );

                    return type_detail_pool_->construct(
                        (type_id_t)type_id_nontype::e_template_class,
                        found_env,
                        nullptr/*not nested*/,
                        std::make_shared<type_detail::template_arg_type>()
                        );
                }

                case kind::type_value::e_function:
                {
                    return type_detail_pool_->construct(
                        (type_id_t)type_id_nontype::e_function,
                        found_env,
                        nullptr/*not nested*/,
                        std::make_shared<type_detail::template_arg_type>()
                        );
                }

                default:
                    std::cerr << "kind: " << debug_string( template_set_env->get_inner_env_symbol_kind() ) << std::endl;
                    assert( false && "[[error]] this template set was not supported yet..." );
                    break;
                }
                break;
            }
#endif



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

    } // namespace semantic_analysis
} // namespace rill
