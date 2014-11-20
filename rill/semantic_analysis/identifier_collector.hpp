//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP
#define RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP

#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>

#include "../ast/visitor.hpp"
#include "../environment/environment_kind.hpp"
#include "../environment/global_environment_fwd.hpp"
#include "messaging.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        //
        class identifier_collector RILL_CXX11_FINAL
            : public ast::ast_visitor_const<
                identifier_collector,
                environment_base_ptr,
                messaging
            >
        {
        public:
            using self_type = identifier_collector;
            using messenger_type = messaging;

        public:
            identifier_collector(
                global_environment_ptr const&,
                boost::filesystem::path const&
                );

        public:
            RILL_VISITOR_OP_DEFAULT

            // statements
            RILL_VISITOR_OP_DECL( ast::module ) const;
            RILL_VISITOR_OP_DECL( ast::statements ) const;
            RILL_VISITOR_OP_DECL( ast::can_be_template_statement ) const;

            RILL_VISITOR_OP_DECL( ast::block_statement ) const;
            RILL_VISITOR_OP_DECL( ast::expression_statement ) const;
            RILL_VISITOR_OP_DECL( ast::function_definition_statement ) const;
            RILL_VISITOR_OP_DECL( ast::variable_declaration_statement ) const;
            RILL_VISITOR_OP_DECL( ast::extern_function_declaration_statement ) const;
            RILL_VISITOR_OP_DECL( ast::extern_class_declaration_statement ) const;
            RILL_VISITOR_OP_DECL( ast::class_definition_statement ) const;
            RILL_VISITOR_OP_DECL( ast::class_function_definition_statement ) const;
            RILL_VISITOR_OP_DECL( ast::class_variable_declaration_statement ) const;

            RILL_VISITOR_OP_DECL( ast::template_statement ) const;

        private:
            inline auto semantic_error(
                message_code const& code,
                ast::const_ast_base_ptr const& ast,
                const_environment_base_ptr const& env,
                boost::format const& message,
                bool const has_appendix = false
                ) const
                -> void
            {
                messaging::semantic_error(
                    get_filepath( env ),
                    code,
                    ast,
                    message,
                    has_appendix
                    );
            }

            inline auto save_appendix_information(
                message_code const& code,
                ast::const_ast_base_ptr const& ast,
                const_environment_base_ptr const& env,
                boost::format const& message
                ) const
                -> void
            {
                messaging::save_appendix_information(
                    get_filepath( env ),
                    code,
                    ast,
                    message
                    );
            }

            auto get_filepath(
                const_environment_base_ptr const& env
                ) const
                -> boost::filesystem::path;

            auto kind_check(
                const_environment_unit_ptr const& env,
                kind::type_value const& kind,
                ast::const_ast_base_ptr const& s,
                environment_base_ptr const& parent_env
                ) const
                -> void;


            //
            auto construct_function(
                ast::function_definition_statement_base_ptr const& s,
                environment_base_ptr const& parent_env,
                bool const is_class_member = false,
                bool const template_not_supported = false
                ) const
                -> void;

            inline auto construct_function_template_not_supported(
                ast::function_definition_statement_base_ptr const& s,
                environment_base_ptr const& parent_env
                ) const
                -> void
            {
                construct_function( s, parent_env, false, true );
            }

            inline auto construct_class_function(
                ast::function_definition_statement_base_ptr const& s,
                environment_base_ptr const& parent_env
                ) const
                -> void
            {
                construct_function( s, parent_env, true );
            }


            //
            auto construct_class(
                ast::class_definition_statement_ptr const& s,
                environment_base_ptr const& parent_env
                ) const
                -> void;


            //
            auto construct_template(
                ast::template_statement_ptr const& s,
                environment_base_ptr const& parent_env
                ) const
                -> void;

        private:
            global_environment_ptr g_env_;
            boost::filesystem::path base_path_;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP*/
