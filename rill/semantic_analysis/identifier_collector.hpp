//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP
#define RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP

#include "../ast/visitor.hpp"
#include "../environment/global_environment_fwd.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        //
        class identifier_collector RILL_CXX11_FINAL
            : public ast::ast_visitor_const<identifier_collector, environment_base_ptr>
        {
        public:
            using self_type = identifier_collector;

        public:
            identifier_collector(
                global_environment_ptr const&
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
            global_environment_ptr g_env_;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP*/
