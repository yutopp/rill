//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP
#define RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP

#include "../tree_visitor_base.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        //
        //
        //
        class identifier_collector RILL_CXX11_FINAL
            : public tree_visitor_base<environment_ptr>
        {
        public:
            RILL_TV_OP_DECL( ast::root_ptr )

            // statement
            RILL_TV_OP_DECL( ast::function_definition_statement_ptr )
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_IDENTIFILER_COLLECTOR_HPP*/
