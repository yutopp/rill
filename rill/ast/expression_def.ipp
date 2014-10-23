//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#if !defined(RILL_AST_EXPRESSION_DEF_IPP) || defined(RILL_AST_FILE_RELOAD)

# ifndef RILL_AST_EXPRESSION_DEF_IPP
#  define RILL_AST_EXPRESSION_DEF_IPP
# endif


#include "detail/def_switch_begin.hpp"
#ifdef RILL_AST_REQ_NS
namespace rill
{
    namespace ast
    {
#endif
        // ========================================
        // expressions
        // ========================================
        RILL_AST_DEF_GROUP( expression )

        RILL_AST_DEF( binary_operator_expression, expression )

        RILL_AST_DEF( element_selector_expression, expression )
        RILL_AST_DEF( subscrpting_expression, expression )
        RILL_AST_DEF( call_expression, expression )
        RILL_AST_DEF( intrinsic_function_call_expression, expression )

        RILL_AST_DEF( term_expression, expression )

        RILL_AST_DEF( id_expression, expression )

        // RILL_AST_DEF( while_expression, expression )
        // RILL_AST_DEF( if_expression, expression )

        //
        RILL_AST_DEF( evaluated_type_expression, expression )

#ifdef RILL_AST_REQ_NS
    } // namespace ast
} // namespace rill
#endif
#include "detail/def_switch_end.hpp"

#endif /*RILL_AST_EXPRESSION_DEF_IPP*/