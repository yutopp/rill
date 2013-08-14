//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "../environment_fwd.hpp"
#include "../tree_visitor_base.hpp"

#include "root_fwd.hpp"

#include "statement.hpp"


namespace rill
{
    namespace ast
    {
        struct root
        {
        public:
            void dispatch( tree_visitor_base const& visitor, environment_ptr const& env ) const
            {
                visitor( *this, env );
            }

        public:
            statement_list statements_;
        };

    } // namespace ast
} // namespace rill
