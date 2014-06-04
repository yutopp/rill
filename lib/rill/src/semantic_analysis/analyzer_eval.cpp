//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/semantic_analysis.hpp>

#include <rill/environment/environment.hpp>
#include <rill/utility/tie.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        // evaluate given expression with sideeffect(instance templates, etc)
        // returns (TYPE of value, and value)
        auto analyzer::eval_expression_as_ctfe(
            ast::expression_ptr const& expression,
            environment_base_ptr const& parent_env
            ) -> std::tuple<
                const_class_symbol_environment_ptr,
                void*
            >
        {
            std::cout << "CTFE of expresison!!!!!!" << std::endl;

            // solve semantics
            auto const& ty_detail = dispatch( expression, parent_env );
            assert( ty_detail != nullptr );

            //
            if ( is_nontype_id( ty_detail->type_id ) ) {
                assert( false && "[ice]" );
            }

            // get environment of the type expresison
            auto const& ty
                = root_env_->get_type_at( ty_detail->type_id );


            std::cout << "KKKKK : " << debug_string( root_env_->get_env_strong_at( ty.class_env_id )->get_symbol_kind() ) << std::endl;

            auto c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    root_env_->get_env_strong_at( ty.class_env_id )
                    );
            assert( c_env != nullptr );

            // eval expression of arguments
            auto evaled_value
                = ctfe_engine_->execute_as_raw_storage( expression, parent_env );
            assert( evaled_value != nullptr );

            return std::forward_as_tuple(
                std::move( c_env ),         // type of eveled_value
                std::move( evaled_value )   //
                );
        }


        // for Template Instance Identifier
        // guarantees the return type is TYPE
        auto analyzer::eval_type_expression_as_ctfe(
            ast::type_expression_ptr const& type_expression,
            environment_base_ptr const& parent_env
            ) -> type_detail_ptr
        {
            std::cout << "TYPE expresison!!!!!!" << std::endl;
            RILL_PP_TIE(
                c_env, evaled_value,
                eval_expression_as_ctfe( type_expression, parent_env )
                );
            assert( c_env != nullptr );

            std::cout << "pass: " << __LINE__ << std::endl;
            std::cout << ": " << c_env << std::endl;
            std::cout << c_env->get_base_name() << std::endl;
            std::cout << "pass: " << __LINE__ << std::endl;

            //
            if ( c_env->get_base_name() != "type" ) {
                assert( false && "[[ice]] value parameter was not supported yet" );
            }

            return static_cast<type_detail_ptr>( evaled_value );
        }

    } // namespace semantic_analysis
} // namespace rill
