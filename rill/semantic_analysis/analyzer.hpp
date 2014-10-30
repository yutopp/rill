//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP
#define RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP

#include <memory>
#include <functional>

#include <boost/optional.hpp>

#include "../ast/visitor.hpp"
#include "../environment/environment_base.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"

#include "../compile_time/llvm_engine/ctfe_engine.hpp"

#include "type_detail.hpp"
#include "type_detail_pool_t.hpp"
#include "type_detail_factory.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        class analyzer final
            : public ast::ast_visitor<analyzer, type_detail_ptr>
        {
            using self_type = analyzer;

        public:
            analyzer(
                environment_base_ptr const&,
                intrinsic_function_action_holder_ptr const&
                );

        public:
            RILL_VISITOR_OP_DEFAULT

            // statement
            RILL_VISITOR_OP_DECL( ast::statements );
            RILL_VISITOR_OP_DECL( ast::block_statement );
          // RILL_VISITOR_OP_DECL( ast::template_statement );
            RILL_VISITOR_OP_DECL( ast::expression_statement );
            RILL_VISITOR_OP_DECL( ast::return_statement );
            RILL_VISITOR_OP_DECL( ast::function_definition_statement );
            RILL_VISITOR_OP_DECL( ast::variable_declaration_statement );
          //RILL_VISITOR_OP_DECL( ast::intrinsic_function_definition_statement );
            RILL_VISITOR_OP_DECL( ast::extern_function_declaration_statement );
            RILL_VISITOR_OP_DECL( ast::class_definition_statement );
            RILL_VISITOR_OP_DECL( ast::class_function_definition_statement );
            RILL_VISITOR_OP_DECL( ast::class_variable_declaration_statement );
            RILL_VISITOR_OP_DECL( ast::test_while_statement );
            RILL_VISITOR_OP_DECL( ast::test_if_statement );

            // expression
            RILL_VISITOR_OP_DECL( ast::element_selector_expression );
            RILL_VISITOR_OP_DECL( ast::subscrpting_expression );
            RILL_VISITOR_OP_DECL( ast::call_expression );
            //RILL_VISITOR_OP_DECL( ast::intrinsic_function_call_expression );
            RILL_VISITOR_OP_DECL( ast::binary_operator_expression );
            RILL_VISITOR_OP_DECL( ast::id_expression );
            RILL_VISITOR_OP_DECL( ast::term_expression );

            // value
            RILL_VISITOR_OP_DECL( ast::nested_identifier_value );
            RILL_VISITOR_OP_DECL( ast::identifier_value );
            RILL_VISITOR_OP_DECL( ast::template_instance_value );

            RILL_VISITOR_OP_DECL( ast::intrinsic::int32_value );
            RILL_VISITOR_OP_DECL( ast::intrinsic::boolean_value );
            RILL_VISITOR_OP_DECL( ast::intrinsic::string_value );
            RILL_VISITOR_OP_DECL( ast::intrinsic::array_value );

        public:
            enum class function_match_level : int
            {
                k_exact_match = 0,
                k_qualifier_conv_match,
                k_implicit_conv_match,
                k_no_match
            };

        public:
            //
            // friends
            //
            friend class code_generator::llvm_ir_generator;

            auto eval_expression_as_ctfe(
                ast::expression_ptr const& expression,
                environment_base_ptr const& parent_env
                ) -> std::tuple<
                    const_class_symbol_environment_ptr,
                    void*
                >;

            auto eval_type_expression_as_ctfe(
                ast::id_expression_ptr const& id_expression,
                environment_base_ptr const& parent_env
                ) -> type_detail_ptr;

            auto eval_type_expression_as_ctfe(
                ast::id_expression_ptr const& id_expression,
                attribute::holder_kind const& holder_kind,
                environment_base_ptr const& parent_env
                ) -> type_detail_ptr;

        public:
            // for Identifier
            auto solve_identifier(
                ast::const_identifier_value_ptr const&,
                environment_base_ptr const&,
                bool const = true
                ) -> type_detail_ptr;

            // for Template Instance Identifier
            auto solve_identifier(
                ast::const_template_instance_value_ptr const&,
                environment_base_ptr const&,
                bool const = true
                ) -> type_detail_ptr;

        private:
            // solve identifier(env, type) and returns type_detail
            auto generic_solve_identifier(
                ast::const_identifier_value_base_ptr const& identifier,
                environment_base_ptr const& parent_env,
                bool const do_not_lookup
                ) -> type_detail_ptr;


        public:
            auto ref_type(
                type_detail_ptr const& ty_detail
                ) const -> type const&;

            auto qualify_type(
                type_detail_ptr const& ty_detail,
                attribute::type_attributes const& type_attr
                ) -> type_detail_ptr;

        private:
            template<typename AstPtr>
            auto bind_type(
                AstPtr const& ast,
                type_detail_ptr const& ty_p
                ) -> type_detail_ptr
            {
                root_env_->bind_type_id_with_ast( ast, ty_p->type_id );
                return ty_p;
            }

        private:
            template<typename F>
            auto resolve_type(
                ast::id_expression_ptr const& id_expression,
                attribute::holder_kind const& holder_kind,
                environment_base_ptr const& parent_env,
                F const& callback
                ) -> type_detail_ptr
            {
                std::cout << "solve_type :before_eval" << std::endl;
                auto const ty_detail
                    = eval_type_expression_as_ctfe( id_expression, holder_kind, parent_env );
                auto const& ty_id = ty_detail->type_id;
                std::cout << "solve_type :after_eval" << std::endl;

                auto const ty = parent_env->get_type_at( ty_id );  // copy Ty...

                auto const& class_env = [&]() {
                    if ( ty.is_incomplete() ) {
                        return static_cast<class_symbol_environment_ptr>( nullptr );

                    } else {
                        auto const p = std::static_pointer_cast<class_symbol_environment>(
                            parent_env->get_env_strong_at( ty.class_env_id )
                            );
                        assert( p != nullptr );
                        assert( p->get_symbol_kind() == kind::type_value::e_class );
                        return p;
                    }
                }();
                std::cout << "solve_type :finished" << std::endl;

                callback( ty_detail, ty, class_env );

                return ty_detail;
            }

            auto infer_param_type_from_arg_type(
                type_detail_ptr,
                const_type_detail_ptr
                )
                -> type_detail_ptr;

            auto try_type_conversion(
                type_id_t const& target_type_id,
                type_id_t const& current_type_id,
                environment_base_ptr const& parent_env
                )
                -> std::tuple<
                    analyzer::function_match_level,
                    function_symbol_environment_ptr
                >;

            auto select_suitable_function(
                std::vector<environment_base_ptr> const& enviroments,
                std::vector<type_detail_ptr> const& arg_types,
                environment_base_ptr const& parent_env
                )
                -> std::tuple<
                    analyzer::function_match_level,
                    boost::optional<std::vector<function_symbol_environment_ptr>>
                >;

            auto instantiate_function_templates(
                multiple_set_environment_ptr const& set_env,
                std::vector<type_detail_ptr> const& arg_types,
                type_detail::template_arg_pointer const& template_args,
                environment_base_ptr const& parent_env
                )
                -> void;

            auto solve_function_return_type_semantics(
                function_symbol_environment_ptr const& f_env
                )
                -> void;

            auto solve_function_overload(
                multiple_set_environment_ptr const& set_env,
                std::vector<type_detail_ptr> const& arg_types,
                type_detail::template_arg_pointer const& template_args,
                environment_base_ptr const& parent_env
                )
                -> function_symbol_environment_ptr;


            auto instantiate_class_templates(
                multiple_set_environment_ptr const& set_env,
                type_detail::template_arg_pointer const& template_args,
                environment_base_ptr const& parent_env
                )
                -> std::vector<class_symbol_environment_ptr>;

            auto solve_class_candidate(
                multiple_set_environment_ptr const& set_env,
                type_detail::template_arg_pointer const& template_args,
                environment_base_ptr const& parent_env
                )
                -> class_symbol_environment_ptr;

            auto declare_template_parameter_variables(
                ast::parameter_list const& template_parameters,
                environment_base_ptr const& inner_env,
                environment_base_ptr const& parent_env
                )
                -> std::vector<variable_symbol_environment_ptr>;

            auto make_template_signature_string(
                std::vector<variable_symbol_environment_ptr> const& decl_template_var_envs
                )
                -> std::string;

            auto make_template_cache_string(
                ast::parameter_list const& template_parameters,
                environment_base_ptr const& inner_env,
                environment_base_ptr const& parent_env
                )
                -> std::vector<variable_symbol_environment_ptr>;

            auto assign_explicit_template_parameters(
                ast::parameter_list const& template_parameters,
                std::vector<variable_symbol_environment_ptr> const& decl_template_var_envs,
                type_detail::template_arg_pointer const& template_args,
                environment_base_ptr const& parent_env
                )
                -> bool;


            auto evaluate_template_args(
                ast::expression_list const& arguments,
                environment_base_ptr const& parent_env
                )
                -> type_detail::template_arg_type;

            // return false, if class is already defined
            auto complete_class(
                ast::class_definition_statement_ptr const& s,
                class_symbol_environment_ptr const& c_env,
                type_detail::template_arg_pointer const& template_args = nullptr,
                boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
                )
                -> bool;

        private:
            auto get_primitive_class_env( std::string const& type_name ) const
                -> class_symbol_environment_ptr;

        private:
            environment_base_ptr root_env_;

            std::shared_ptr<type_detail_pool_t> type_detail_pool_;
            std::shared_ptr<type_detail_factory> type_detail_factory_;
            std::shared_ptr<compile_time::llvm_engine::ctfe_engine> ctfe_engine_;

        private:
            class builtin_class_envs_cache;
            std::shared_ptr<builtin_class_envs_cache> builtin_class_envs_cache_;
        };


        //
        template<typename EnvPtr>
        inline auto to_unique_class_env( EnvPtr const& env )
            -> class_symbol_environment_ptr
        {
            if ( env == nullptr ) {
                return nullptr;
            }
            if ( env->get_symbol_kind() != kind::type_value::e_multi_set ) {
                return nullptr;
            }

            auto const& multi_set_env = cast_to<multiple_set_environment>( env );
            if ( multi_set_env == nullptr ) {
                return nullptr;
            }
            if ( multi_set_env->get_representation_kind() != kind::type_value::e_class ) {
                return nullptr;
            }

            auto class_env = multi_set_env->template get_unique_environment<class_symbol_environment>();
            if ( class_env == nullptr ) {
                return nullptr;
            }

            return class_env;
        }

        //
        auto make_mangled_name(
            const_class_symbol_environment_ptr const& c_env,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
            )
            -> std::string;
        //
        auto make_mangled_name(
            const_class_symbol_environment_ptr const& c_env,
            attribute::type_attributes const& attr,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
            )
            -> std::string;

        auto make_mangled_name(
            const_function_symbol_environment_ptr const& f_env,
            boost::optional<std::reference_wrapper<std::string const>> const& template_signature = boost::none
            )
            -> std::string;

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_ANALYZER_HPP*/
