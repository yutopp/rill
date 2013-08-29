//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_LIST_IDENTIFIER_VISITOR_HPP
#define RILL_SEMANTIC_ANALYSIS_LIST_IDENTIFIER_VISITOR_HPP

#include <memory>

#include "../tree_visitor_base.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        //
        //
        //
        class list_identifier_visitor RILL_CXX11_FINAL
            : public tree_visitor_base<environment_ptr>
        {
        public:
            // statement
            RILL_TV_OP_DECL( ast::function_definition_statement )
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_LIST_IDENTIFIER_VISITOR_HPP*/
