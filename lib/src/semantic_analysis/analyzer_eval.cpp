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
                type_detail_ptr,
                void*
            >
        {
            rill_dout << "CTFE of expresison!!!!!!" << std::endl;

            // solve semantics
            // this ty_detail denotes type of "value(includes types)"
            auto ty_detail = dispatch( expression, parent_env );
            assert( ty_detail != nullptr );

            //
            if ( is_nontype_id( ty_detail->type_id ) ) {
                assert( false && "[ice]" );
            }

            // get environment of the type expresison
            auto const& ty
                = g_env_->get_type_at( ty_detail->type_id );

            rill_dout << "KKKKK : " << debug_string( g_env_->get_env_at_as_strong_ref( ty.class_env_id )->get_symbol_kind() ) << std::endl;

            auto c_env
                = std::static_pointer_cast<class_symbol_environment const>(
                    g_env_->get_env_at_as_strong_ref( ty.class_env_id )
                    );
            assert( c_env != nullptr );

            // eval expression of arguments
            // "value"(typeinfo, value of integer, ...)
            auto evaled_value
                = ctfe_engine_->execute_as_raw_storage( expression, parent_env );
            assert( evaled_value != nullptr );

            return std::forward_as_tuple(
                std::move( c_env ),         // type of eveled_value
                std::move( ty_detail ),
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
            rill_dout << "TYPE expresison!!!!!!" << std::endl;
            RILL_PP_TIE(
                c_env, ty_detail, evaled_value,
                eval_expression_as_ctfe( id_expression, parent_env )
                );
            assert( c_env != nullptr );

            rill_dout << std::endl
                      << ">>>>> pass(eval_type_expression_as_ctfe): " << __LINE__ << std::endl
                      << "  env ptr     : " << c_env << std::endl
                      << "  env basename: " << c_env->get_base_name() << std::endl
                      << "  env qualname: " << c_env->get_qualified_name() << std::endl
                      << ">>>>> pass(eval_type_expression_as_ctfe): " << __LINE__ << std::endl
                      << std::endl;

            //
            if ( c_env->get_base_name() != "type" ) {
                assert( false && "[[ice]] not 'type' type" );
            }

            // real type(int, double, etc...)
            auto inner_ty_d = static_cast<type_detail_ptr>( evaled_value );

            rill_dout << "APPEND attributes" << std::endl;
            auto ty = g_env_->get_type_at( inner_ty_d->type_id ); // make copy
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
            inner_ty_d->type_id = g_env_->make_type_id(
                ty.class_env_id,
                ty.attributes
                );

            // copy templete args!
            inner_ty_d->template_args = ty_detail->template_args;

            return inner_ty_d;
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
                    auto const& value_holder
                        = static_cast<raw_value_holder_ptr>( evaled_value );
                    auto const& inner
                        = static_cast<bool const*>(
                            value_holder->ptr_to_raw_value.get()
                            );

                    return std::make_shared<ast::term_expression>(
                        std::make_shared<ast::intrinsic::boolean_value>(
                            *inner
                            )
                        );
                }

                case class_builtin_kind::k_int32:
                {
                    auto const& value_holder
                        = static_cast<raw_value_holder_ptr>( evaled_value );
                    auto const& inner
                        = static_cast<std::int32_t const*>(
                            value_holder->ptr_to_raw_value.get()
                            );

                    return std::make_shared<ast::term_expression>(
                        std::make_shared<ast::intrinsic::int32_value>(
                            *inner
                            )
                        );
                }

                default:
                {
                    rill_dout << orig_c_env->get_base_name() << std::endl;
                    assert( false && "[[ice]] this value type was not supported currently." );
                    return nullptr;
                }
                } // switch
            }();

            substituted_ast->line = expression->line;
            substituted_ast->column = expression->column;

            rill_dout << expression->line << std::endl
                      << expression->column << std::endl;

            // substitute expression
            expression.swap( substituted_ast );

            rill_dout << "--" << std::endl
                      << expression->line << std::endl
                      << expression->column << std::endl;

            // rebind
            bind_type( expression, orig_ty_d );
        }

    } // namespace semantic_analysis
} // namespace rill
