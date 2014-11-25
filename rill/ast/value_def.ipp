//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#if !defined(RILL_AST_VALUE_DEF_IPP) || defined(RILL_AST_FILE_RELOAD)

# ifndef RILL_AST_VALUE_DEF_IPP
#  define RILL_AST_VALUE_DEF_IPP
# endif


#include "detail/def_switch_begin.hpp"
#if defined( RILL_AST_REQ_NS )
namespace rill
{
    namespace ast
    {
#endif
        // ========================================
        // values
        // ========================================
        RILL_AST_DEF_GROUP( value )

        RILL_AST_DEF_IN_NAMESPACE( intrinsic, value_base, value )

        RILL_AST_DEF_IN_NAMESPACE( intrinsic, symbol_value, value )
        RILL_AST_DEF_IN_NAMESPACE( intrinsic, int32_value, value )
        RILL_AST_DEF_IN_NAMESPACE( intrinsic, float_value, value )
        RILL_AST_DEF_IN_NAMESPACE( intrinsic, boolean_value, value )
        RILL_AST_DEF_IN_NAMESPACE( intrinsic, string_value, value )
        RILL_AST_DEF_IN_NAMESPACE( intrinsic, array_value, value )

        //
        RILL_AST_DEF( identifier_value_base, value )
        RILL_AST_DEF( identifier_value, value )
        RILL_AST_DEF( template_instance_value, value )
        RILL_AST_DEF( nested_identifier_value, value )

        //
        RILL_AST_DEF( captured_value, value )

#if defined( RILL_AST_REQ_NS )
    } // namespace ast
} // namespace rill
#endif
#include "detail/def_switch_end.hpp"

#endif /*RILL_AST_VALUE_DEF_IPP*/