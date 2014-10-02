//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_TREE_VISITOR_RESULT_T_HPP
#define RILL_AST_DETAIL_TREE_VISITOR_RESULT_T_HPP

#include <type_traits>

#include "../value_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../statement_fwd.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // --
            template<typename ReturnT, typename NodeT>
            struct tree_visitor_result;

            template<typename ReturnT>
            struct tree_visitor_result<ReturnT, ast::statement> { using type = void; };

            template<typename ReturnT>
            struct tree_visitor_result<ReturnT, ast::expression> { using type = ReturnT; };

            template<typename ReturnT>
            struct tree_visitor_result<ReturnT, ast::value> { using type = ReturnT; };

            //
            template<typename ReturnT, typename NodeT>
            using tree_visitor_result_t = typename tree_visitor_result<
                ReturnT, ast_base_type<typename std::decay<NodeT>::type>
                >::type;

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_TREE_VISITOR_RESULT_T_HPP*/
