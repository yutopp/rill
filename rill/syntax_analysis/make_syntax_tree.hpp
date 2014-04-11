//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_MAKE_SYNTAX_TREE_HPP
#define RILL_SYNTAX_ANALYSIS_MAKE_SYNTAX_TREE_HPP

#include "../ast/ast.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        auto make_syntax_tree( ast::native_string_t const& source ) -> ast::statements_ptr;

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_MAKE_SYNTAX_TREE_HPP*/
