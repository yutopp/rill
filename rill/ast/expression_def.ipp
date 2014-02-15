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


#include "detail/on.hpp"
namespace rill
{
    namespace ast
    {
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        //
        // expressions
        //
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        RILL_AST_DEF_GROUP( expression )

        RILL_AST_DEF( binary_operator_expression, expression )

        RILL_AST_DEF( element_selector_expression, expression )
        RILL_AST_DEF( subscrpting_expression, expression )
        RILL_AST_DEF( call_expression, expression )
        RILL_AST_DEF( intrinsic_function_call_expression, expression )


        RILL_AST_DEF( term_expression, expression )


        RILL_AST_DEF( type_expression, expression )
        RILL_AST_DEF( type_identifier_expression, expression )
        RILL_AST_DEF( compiletime_return_type_expression, expression )
    }
}
#include "detail/off.hpp"

#endif /*RILL_AST_EXPRESSION_DEF_IPP*/