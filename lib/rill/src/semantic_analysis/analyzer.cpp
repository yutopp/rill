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

#include <unordered_map>


namespace rill
{
    namespace semantic_analysis
    {
        class analyzer::builtin_class_envs_cache
        {
            friend analyzer;

        public:
            builtin_class_envs_cache( environment_base_ptr const& root_env )
            {
                auto install_primitive_class = [&]( std::string const& type_name ) mutable {
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
        {}

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
            assert( c_env->is_complete() );

            std::string s;
            s += std::to_string( c_env->get_mangled_name().size() );
            s += c_env->get_mangled_name();

            s += [&]() {
                switch( attr.quality )
                {
                case attribute::quality_kind::k_val:
                    return "VAL";
                case attribute::quality_kind::k_ref:
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
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& inner_env,
            environment_base_ptr const& parent_env,
            std::vector<environment_base_ptr>& declared_envs
            )
            -> void
        {
            assert( declared_envs.size() == template_parameters.size() );

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
                    std::cout << "template parameter decl " << i << std::endl;
                    solve_type(
                        this,
                        template_parameter.decl_unit.init_unit.type,
                        parent_env,
                        [&]( type_detail_ptr const& ty_d,
                             type const& ty,
                             class_symbol_environment_ptr const& class_env
                            ) {
                            auto attr = ty.attributes;
                            attr <<= template_parameter.quality;

                            // declare the template parameter into function env as variable
                            auto const& v_env
                                = inner_env->construct(
                                    kind::k_variable,
                                    template_parameter.decl_unit.name,
                                    nullptr/*TODO: change to valid ptr to ast*/,
                                    class_env,
                                    attr
                                    );

                            declared_envs[i] = v_env;
                        });

                } else {
                    // TODO: set as TYPE
                    assert( false && "TODO: it will be type" );
                }
            }
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

            std::vector<environment_base_ptr> decl_arg_holder( template_parameter_list.size() );
            std::cout << "TEMPLATE param size is " << template_parameter_list.size() << std::endl;

            // 1. declare template parameters
            declare_template_parameter_variables(
                template_parameter_list,
                template_args,
                inner_env,
                parent_env,
                decl_arg_holder
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
                = type_class_env->make_type_id( type_class_env, determine_type_attributes() );

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
            case attribute::quality_kind::k_ref:
            {
                // ref to ref
                result_attr <<= attribute::quality_kind::k_ref;
                break;
            }

            case attribute::quality_kind::k_val:
            {
                // ref to val
                result_attr <<= attribute::quality_kind::k_val;
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
            case attribute::quality_kind::k_ref:
            {
                // val to ref
                result_attr <<= attribute::quality_kind::k_ref;
                break;
            }

            case attribute::quality_kind::k_val:
            {
                // ref to val
                result_attr <<= attribute::quality_kind::k_val;
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
            case attribute::quality_kind::k_ref:
                return qualifier_conversion_from_ref( parameter_attributes, argument_attributes );

            case attribute::quality_kind::k_val:
                return qualifier_conversion_from_val( parameter_attributes, argument_attributes );

            default:
                assert( false );
            }

            return boost::none;
        }




        auto analyzer::solve_function_overload(
            multiple_set_environment_ptr const& set_env,
            std::vector<type_detail_ptr> const& arg_types,
            type_detail::template_arg_pointer const& template_args,
            environment_base_ptr const& parent_env
            )
            -> function_symbol_environment_ptr
        {
            //
            //
            type_id_list_t holder( arg_types.size() );
            std::vector<function_symbol_environment_ptr> f_candidate_envs;

            std::cout << "!!!! overload solving: " << set_env->get_name() << std::endl
                      << "!!!! function candidate num: " << set_env->get_normal_environments().size() << std::endl;
            //
            assert( set_env->get_representation_kind() == kind::type_value::e_function );

            std::multimap<int, function_symbol_environment_ptr> solved_function_envs;

            // TODO: add hit cache

            // TODO: skip normal function search, if template_args is NOT nullptr

            // 0: exact match
            // 1: qualifier conversion match
            // 2: implicit conversion match
            // 3: no match
            int best_matched_level = 3;
            int worst_matched_level = 0;

            // first, see normal envs
            for( auto&& env : set_env->get_normal_environments() ) {
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
                if ( f_env_parameter_type_ids.size() != arg_types.size() ) continue;

                int function_match_level = 0;   // assume the function is "exact match".
                // type_id_list_t type_id_holder( arg_types.size() );

                // check each arguments/parameters
                for( std::size_t i=0; i<arg_types.size(); ++i ) {
                    auto const& param_type_id = f_env->get_parameter_type_ids()[i];
                    auto const& arg_type_id = arg_types[i]->type_id;

                    if ( param_type_id == arg_type_id ) {
                        // exact match

                        // holder[i] = arg_type_id;

                        // update level
                        function_match_level = std::max( function_match_level, 0 );

                    } else {
                        // try to type conversion
                        auto const& param_type = f_env->get_type_at( param_type_id );
                        auto const& arg_type = f_env->get_type_at( arg_type_id );

                        if ( param_type.class_env_id == arg_type.class_env_id ) {
                            // check quarity conversion
                            if ( auto&& attribute_opt = qualifier_conversion( param_type.attributes, arg_type.attributes ) ) {
                                // qualifier conversion match
/*
                                auto const& c_env
                                    = root_env_->get_env_at_as_strong_ref<class_symbol_environment const>( param_type.class_env_id );
                                assert( c_env != nullptr );
                                auto const& converted_type_id
                                    = root_env_->make_type_id( c_env, *attribute_opt );
*/
                                // holder[i] = converted_type_id;

                                // update level
                                function_match_level = std::max( function_match_level, 1 );

                            } else {
                                // unmatched
                            }

                        } else {
                            // TODO: implement implicit conversion match
                            // currentry failed immediately

                            function_match_level = std::max( function_match_level, 3 );
                        }

                    }

                    // this function will be never matched
                    if ( function_match_level == 3 ) break;
                } // for

                // not matched: failed
                if ( function_match_level == 3 ) continue;


                f_candidate_envs.push_back( f_env );
                solved_function_envs.emplace( function_match_level, f_env );

                //
                best_matched_level = std::min( function_match_level, best_matched_level );
                worst_matched_level = std::max( function_match_level, worst_matched_level );
            } // for [normal environment]


            std::cout << " !== overload ======================================================" << std::endl
                      << "best match: " << best_matched_level << std::endl
                      << "worst match: " << worst_matched_level << std::endl;
            for( auto&& e : f_candidate_envs ) {
                std::cout << std::endl
                          << "  !!!!! condidate found >>> " << std::endl
                          << std::endl;
            }


            // check overloaded set


            assert( f_candidate_envs.size() > 0 );
            auto&& selected_function = f_candidate_envs.at( 0 );

            return selected_function;
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
                        = type_class_env->make_type_id( type_class_env, determine_type_attributes() );

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
                auto const& variable_env
                    = std::static_pointer_cast<variable_symbol_environment>( found_env );

                // memoize
                std::cout << "()memoed.variable" << std::endl;
                variable_env->connect_from_ast( identifier );

                // class
                // variable_env->get_type_id();

                return type_detail_pool_->construct(
                    variable_env->get_type_id(),
                    variable_env
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
