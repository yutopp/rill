//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_AST_BASE_HPP
#define RILL_AST_AST_BASE_HPP

#include <cstddef>
#include <memory>


namespace rill
{
    namespace ast
    {
        class ast_base
        {
        public:
            std::size_t line, column, length;
        };

        typedef std::shared_ptr<ast_base> ast_base_ptr;
        typedef std::shared_ptr<ast_base const> const_ast_base_ptr;

    } // namespace ast
} // namespace rill

#endif /*RILL_AST_AST_BASE_HPP*/
