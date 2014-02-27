//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP
#define RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP

#include "../ast/detail/tree_visitor_base.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        enum class collection_type
        {
            e_normal,
            e_builtin
        };


        //
        class identifier_collector RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<identifier_collector, environment_base_ptr>
        {
        public:
            identifier_collector(
                collection_type const& ct = collection_type::e_normal
                )
                : ct_( ct )
            {}

        public:
            // statements
            RILL_TV_OP_DECL( ast::statements )
            RILL_TV_OP_DECL( ast::can_be_template_statement )

            RILL_TV_OP_DECL( ast::block_statement )
            RILL_TV_OP_DECL( ast::expression_statement )            
            RILL_TV_OP_DECL( ast::function_definition_statement )
            RILL_TV_OP_DECL( ast::variable_declaration_statement )
            RILL_TV_OP_DECL( ast::extern_function_declaration_statement )
            RILL_TV_OP_DECL( ast::class_definition_statement )
            RILL_TV_OP_DECL( ast::class_function_definition_statement )
            RILL_TV_OP_DECL( ast::class_variable_declaration_statement )

            RILL_TV_OP_DECL( ast::template_statement )

            RILL_TV_OP_FAIL

        public:
            inline auto is_builtin() const
                -> bool
            {
                return ct_ == collection_type::e_builtin;
            }

        private:
            collection_type ct_;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP*/
