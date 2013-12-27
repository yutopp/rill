//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <memory>
#include <vector>

#include "detail/specifier.hpp"

#include "ast_base.hpp"


namespace rill
{
    namespace ast
    {
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        //
        // statements
        //
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        RILL_AST_FWD_DECL( statement, statement )

        typedef std::vector<statement_ptr>  statement_list;
        RILL_AST_FWD_DECL( statements, statement )
        
        RILL_AST_FWD_DECL( block_statement, statement )

        template<typename Target>
        struct template_statement;


        RILL_AST_FWD_DECL( empty_statement, statement )
        RILL_AST_FWD_DECL( expression_statement, statement )


        RILL_AST_FWD_DECL( function_definition_statement_base, statement )
        RILL_AST_FWD_DECL( function_definition_statement, statement )
        RILL_AST_FWD_DECL( intrinsic_function_definition_statement, statement )
        RILL_AST_FWD_DECL( class_function_definition_statement, statement )

        RILL_AST_FWD_DECL( class_definition_statement, statement )


        RILL_AST_FWD_DECL( return_statement, statement )
        RILL_AST_FWD_DECL( jit_statement, statement )


        RILL_AST_FWD_DECL( extern_statement_base, statement )
        RILL_AST_FWD_DECL( extern_function_declaration_statement, statement )

        RILL_AST_FWD_DECL( variable_declaration_statement, statement )
        RILL_AST_FWD_DECL( class_variable_declaration_statement, statement )

        //// TEST
        RILL_AST_FWD_DECL( test_while_statement, statement )
        RILL_AST_FWD_DECL( test_if_statement, statement )



        

    } // namespace ast
} // namespace rill
