//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_STATEMENT_FWD_HPP
#define RILL_AST_STATEMENT_FWD_HPP

#include <vector>

#include "detail/base_type.hpp"
#include "statement_def.ipp"


namespace rill
{
    namespace ast
    {
        namespace element
        {
            using statement_list = std::vector<statement_ptr>;
        }

    } // namespace ast
} // namespace rill

#endif /*RILL_AST_STATEMENT_FWD_HPP*/
