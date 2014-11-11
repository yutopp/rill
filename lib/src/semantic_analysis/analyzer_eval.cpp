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
            debug_out << "CTFE of expresison!!!!!!" << std::endl;

            // solve semantics
            auto const& ty_detail = dispatch( expression, parent_env );
            assert( ty_detail != nullptr );

            //
            if ( is_nontype_id( ty_detail->type_id ) ) {
                assert( false && "[ice]" );
            }

            // get environment of the type expresison
            auto const& ty
                = g_env_->get_type_at( ty_detail->type_id );

            debug_out << "KKKKK : " << debug_string( g_env_->get_env_at_as_strong_ref( ty.class_env_id )->get_symbol_kind() ) << std::endl;

            auto c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    g_env_->get_env_at_as_strong_ref( ty.class_env_id )
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
            ast::id_expression_ptr const& id_expression,
            attribute::type_attributes const& rap_attr,
            environment_base_ptr const& parent_env
            ) -> type_detail_ptr
        {
            debug_out << "TYPE expresison!!!!!!" << std::endl;
            RILL_PP_TIE(
                c_env, evaled_value,
                eval_expression_as_ctfe( id_expression, parent_env )
                );
            assert( c_env != nullptr );

            debug_out << "pass: " << __LINE__ << std::endl
                      << ": " << c_env << std::endl
                      << c_env->get_base_name() << std::endl
                      << "pass: " << __LINE__ << std::endl;

            //
            if ( c_env->get_base_name() != "type" ) {
                assert( false && "[[ice]] not 'type' type" );
            }

            auto ty_d = static_cast<type_detail_ptr>( evaled_value );

            debug_out << "APPEND attributes" << std::endl;
            auto ty = g_env_->get_type_at( ty_d->type_id ); // make copy
            // force overwrite holder type(if specified)
            // other attributes are filled
            ty.attributes = overlap_empty_attr(
                attribute::detail::overwrite_by_nonempty(
                    ty.attributes,
                    rap_attr.quality
                    ),
                rap_attr
                );

            // update type id
            ty_d->type_id = g_env_->make_type_id(
                ty.class_env_id,
                ty.attributes
                );

            return ty_d;
        }

        auto analyzer::eval_type_expression_as_ctfe(
            ast::id_expression_ptr const& id_expression,
            attribute::holder_kind const& holder_kind,
            environment_base_ptr const& parent_env
            ) -> type_detail_ptr
        {
            return eval_type_expression_as_ctfe(
                id_expression,
                attribute::make( holder_kind, attribute::modifiability_kind::k_const ),
                parent_env
                );
        }

        auto analyzer::substitute_by_ctfed_node(
            ast::expression_ptr& expression,
            type_detail_ptr const& orig_ty_d,
            environment_base_ptr const& parent_env
            ) -> void
        {
            // regared: expression is already checked that semantics is valid

            auto const& orig_ty
                = g_env_->get_type_at( orig_ty_d->type_id );
            auto const& orig_c_env
                = g_env_->get_env_at_as_strong_ref<class_symbol_environment const>(
                    orig_ty.class_env_id
                    );
            assert( orig_c_env != nullptr );

            // eval expression of arguments
            auto evaled_value
                = ctfe_engine_->execute_as_raw_storage( expression, parent_env );
            assert( evaled_value != nullptr );

            auto substituted_ast = [&orig_c_env, &evaled_value]() -> ast::expression_ptr {
                switch( orig_c_env->get_builtin_kind() ) {
                case class_builtin_kind::k_type:
                {
                    auto const& ty_detail
                        = static_cast<type_detail_ptr>( evaled_value );
                    return std::make_shared<ast::evaluated_type_expression>(
                        ty_detail->type_id
                        );
                }

                case class_builtin_kind::k_bool:
                {
                    auto const& inner
                        = static_cast<bool const*>( evaled_value );

                    return std::make_shared<ast::term_expression>(
                        std::make_shared<ast::intrinsic::boolean_value>(
                            *inner
                            )
                        );
                }

                default:
                {
                    debug_out << orig_c_env->get_base_name() << std::endl;
                    assert( false && "[[ice]] this value type was not supported currently." );
                    return nullptr;
                }
                } // switch
            }();

            // substitute expression
            expression.swap( substituted_ast );

            // rebind
            bind_type( expression, orig_ty_d );
        }

    } // namespace semantic_analysis
} // namespace rill
