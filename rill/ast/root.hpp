//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "../environment/environment_fwd.hpp"

#include "detail/tree_visitor_base.hpp"
#include "detail/dispatch_assets.hpp"

#include "root_fwd.hpp"

#include "statement.hpp"


namespace rill
{
    namespace ast
    {
        struct root RILL_CXX11_FINAL
        {
        public:
            RILL_AST_ADAPT_VISITOR( root )

        public:
            root( statement_list&& sl )
                : statements_( std::move( sl ) )
            {}

        public:
            statement_list statements_;
        };

    } // namespace ast
} // namespace rill
