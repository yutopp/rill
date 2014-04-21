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


namespace rill
{
    namespace semantic_analysis
    {
        //
        analyzer::analyzer(
            environment_base_ptr const& root_env,
            intrinsic_function_action_holder_ptr const& holder
            )
            : root_env_( root_env )
            , type_detail_pool_( std::make_shared<type_detail_pool_t>() )
            , ctfe_engine_( compile_time::llvm_engine::make_ctfe_engine( this, root_env, holder, type_detail_pool_ ) )
        {}



        auto analyzer::ref_type(
            type_detail_ptr const& ty_detail
            ) const -> type const&
        {
            return root_env_->get_type_at( ty_detail->type_id );
        }


        auto analyzer::qualify_type(
            type_detail_ptr const& ty_detail,
            attribute::type_attributes const& type_attr
            ) -> type_detail_ptr
        {
            // TODO: add duplicate check
            auto const& t = ref_type( ty_detail );

            auto const& qualified_type_id
                = root_env_->make_type_id(
                    t.class_env_id,
                    type_attr
                    );

            return type_detail_pool_->construct(
                qualified_type_id,
                ty_detail->target_env,
                ty_detail->nest,
                ty_detail->template_args
                );
        }


        //
        auto analyzer::tp(
            ast::parameter_list const& template_parameter_list,
            type_detail::template_arg_pointer const& template_args,
            single_identifier_environment_base_ptr const& inner_env,
            environment_base_ptr const& parent_env
            )
            -> void
        {
            // template parameters
            // import template parameter's variables with instantiation!

            std::vector<environment_base_ptr> decl_arg_holder( template_parameter_list.size() );
            std::cout << "TEMPLATE param size is " << template_parameter_list.size() << std::endl;

            for( std::size_t i=0; i<template_parameter_list.size(); ++i ) {
                auto const& template_parameter = template_parameter_list.at( i );

                //
                // declare template parameters
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

                            decl_arg_holder[i] = v_env;
                        });

                } else {
                    // TODO: set as TYPE
                    assert( false && "TODO: it will be type" );
                }

                // TODO: add error check to varidate param and arg size...

                std::cout << "pp i = " << i << " / " << template_args->size() << std::endl;

                //
                // template arguments
                // if the template argument was passed explicitly, save argument value
                // if NOT, it will be deduced after that...
                if ( i <= template_args->size() ) {

                    std::cout << "TEMPLATE ARGS!! " << i << std::endl;
                    auto const& template_var_env = decl_arg_holder[i];
                    auto const& template_arg = template_args->at( i );

                    {
                        // DEBUG
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

                } else {
                    // TODO: error...?
                    assert( false );
                }
            }
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

            std::cout << "template class! : Arg num~ "
                      << target_ty_detail->template_args->size()
                      << std::endl;

            // take a template set
            auto const& template_set_env
                = std::static_pointer_cast<template_set_environment>(
                    target_ty_detail->target_env
                    );

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
                    = template_set_env->allocate_env<class_symbol_environment>();

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
            auto const& type_class_env
                = root_env_->lookup( ast::make_identifier( "type" ) );
            assert( type_class_env != nullptr );  // literal type must exist

            target_ty_detail->type_id
                = type_class_env->make_type_id( type_class_env, determine_type_attributes() );

            target_ty_detail->target_env
                = type_class_env;

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


    } // namespace semantic_analysis
} // namespace rill
