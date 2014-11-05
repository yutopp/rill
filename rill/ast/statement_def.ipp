//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#if !defined(RILL_AST_STATEMENT_DEF_IPP) || defined(RILL_AST_FILE_RELOAD)

# ifndef RILL_AST_STATEMENT_DEF_IPP
#  define RILL_AST_STATEMENT_DEF_IPP
# endif


#include "detail/def_switch_begin.hpp"
#ifdef RILL_AST_REQ_NS
namespace rill
{
    namespace ast
    {
#endif
        // ========================================
        // statements
        // ========================================
        RILL_AST_DEF_GROUP( statement )

        RILL_AST_DEF( module, statement )
        RILL_AST_DEF( statements, statement )

        RILL_AST_DEF( expression_statement, statement )
        RILL_AST_DEF( block_statement, statement )
        RILL_AST_DEF( empty_statement, statement )

        RILL_AST_DEF( can_be_template_statement, statement )
        RILL_AST_DEF( template_statement, statement )

        RILL_AST_DEF( function_definition_statement_base, statement )
        RILL_AST_DEF( function_definition_statement, statement )
        RILL_AST_DEF( class_function_definition_statement, statement )
        RILL_AST_DEF( extern_function_declaration_statement, statement )

        RILL_AST_DEF( class_definition_statement, statement )
        RILL_AST_DEF( extern_class_declaration_statement, statement )

        RILL_AST_DEF( variable_declaration_statement, statement )
        RILL_AST_DEF( class_variable_declaration_statement, statement )

        RILL_AST_DEF( return_statement, statement )

        RILL_AST_DEF( import_statement, statement )

        //// TEST
        RILL_AST_DEF( test_while_statement, statement )
        RILL_AST_DEF( test_if_statement, statement )
#ifdef RILL_AST_REQ_NS
    } // namespace ast
} // namespace rill
#endif
#include "detail/def_switch_end.hpp"

#endif /*RILL_AST_STATEMENT_DEF_IPP*/