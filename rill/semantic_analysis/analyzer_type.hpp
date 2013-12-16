//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "../environment/environment.hpp"

#include "../ast/root.hpp"
#include "../ast/statement.hpp"
#include "../ast/expression.hpp"
#include "../ast/value.hpp"


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


        template<typename TypeIds, typename EnvPtr, typename ResultCallbackT>
        static auto overload_solver(
            TypeIds const& arg_type_ids,
            std::shared_ptr<has_parameter_environment<function_symbol_environment>> const& generic_function_env,
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
            //std::cout << "...resolving function : " << identifier->get_inner_symbol()->to_native_string() << std::endl;


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




        template<typename TypeIds, typename EnvPtr>
        static inline auto overload_solver(
            TypeIds const& arg_type_ids ,
            std::shared_ptr<has_parameter_environment<function_symbol_environment>> const& generic_function_env,
            EnvPtr const& env
            )
            -> function_symbol_environment_ptr
        {
            return overload_solver(
                arg_type_ids,
                generic_function_env,
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

        template<typename TypeIds, typename EnvPtr>
        static inline auto overload_solver_allow_no_entry(
            TypeIds const& arg_type_ids ,
            std::shared_ptr<has_parameter_environment<function_symbol_environment>> const& generic_function_env,
            EnvPtr const& env
            )
            -> function_symbol_environment_ptr
        {
            return overload_solver(
                arg_type_ids,
                generic_function_env,
                env,
                []( std::vector<function_symbol_environment_ptr> const& f_candidate_envs )-> function_symbol_environment_ptr
                {
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

    } // namespace semantic_analysis
} // namespace rill
