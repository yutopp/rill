//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/debug/ast_printer.hpp>
#include <rill/environment/environment.hpp>

#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace debug
    {
        //
        // Root Scope
        //
        RILL_TV_OP( ast_printer, ast::statements, s, _ )
        {
            o( "statements", [&]( std::string const& ss ) {
                    for( auto const& ss : s->statement_list_ )
                        dispatch( ss );
                } );
        }


        RILL_TV_OP( ast_printer, ast::block_statement, s, _ )
        {
            o( "block_statement", [&]( std::string const& ss ) {
                    dispatch( s->statements_ );
                } );
        }


        //
        // Expression Statement
        //
        RILL_TV_OP( ast_printer, ast::expression_statement, s, _ )
        {
            o( "expression_statement", [&]( std::string const& ss ) {
                    dispatch( s->expression_ );
                } );
        }



        //
        //
        //
        RILL_TV_OP( ast_printer, ast::return_statement, s, _ )
        {
            o( "return_statement", [&]( std::string const& ss ) {
                } );
        }



        //
        //
        //
        RILL_TV_OP( ast_printer, ast::function_definition_statement, s, _ )
        {
            o( "function_definition_statement", [&]( std::string const& ss ) {
                    dispatch( s->inner_ );
                } );
        }




        //
        //
        //
        RILL_TV_OP( ast_printer, ast::class_function_definition_statement, s, _ )
        {
            o( "class_function_definition_statement", [&]( std::string const& ss ) {
                    dispatch( s->inner_ );
                } );
        }





        RILL_TV_OP( ast_printer, ast::class_definition_statement, s, _ )
        {
            o( "class_definition_statement", [&]( std::string const& ss ) {
                    dispatch( s->inner_ );
                } );
        }














        RILL_TV_OP( ast_printer, ast::intrinsic_function_definition_statement, s, _ )
        {
            o( "intrinsic_function_daginition_statement", [&]( std::string const& ss ) {
                    dispatch( s->inner_ );
                } );
        }


        //
        //
        //
        RILL_TV_OP( ast_printer, ast::variable_declaration_statement, s, _ )
        {
            o( "variable_declaration_statement", [&]( std::string const& ss ) {
                } );
        }




        //
        //
        //
        RILL_TV_OP( ast_printer, ast::class_variable_declaration_statement, s, _ )
        {
            o( "class_variable_declaration_statement", [&]( std::string const& ss ) {
                } );
        }




        RILL_TV_OP( ast_printer, ast::extern_function_declaration_statement, s, _ )
        {
            o( "extern_function_declaration_statement", [&]( std::string const& ss ) {
                } );
        }


        RILL_TV_OP( ast_printer, ast::template_statement, s, _ )
        {
            o( "template_statement", [&]( std::string const& ss ) {
                    dispatch( s->get_inner_statement() );
                } );
        }


        RILL_TV_OP( ast_printer, ast::test_while_statement, s, _ )
        {
            o( "test_while_statement", [&]( std::string const& ss ) {
                    dispatch( s->body_statement_ );
                } );
        }






        RILL_TV_OP( ast_printer, ast::test_if_statement, s, _ )
        {
            o( "test_if_statement", [&]( std::string const& ss ) {
                } );
        }






        RILL_TV_OP( ast_printer, ast::binary_operator_expression, e, _ )
        {
        }



        //
        //
        //
        RILL_TV_OP( ast_printer, ast::element_selector_expression, e, _ )
        {
        }



        //
        //
        //
        RILL_TV_OP( ast_printer, ast::call_expression, e, _ )
        {
        }


        // TODO: change name to native code injection expression
        RILL_TV_OP( ast_printer, ast::intrinsic_function_call_expression, e, _ )
        {
        }


        RILL_TV_OP( ast_printer, ast::term_expression, e, _ )
        {
        }




        RILL_TV_OP( ast_printer, ast::nested_identifier_value, v, _ )
        {
        }


        RILL_TV_OP( ast_printer, ast::identifier_value, v, _ )
        {
        }


        RILL_TV_OP( ast_printer, ast::literal_value, v, _ )
        {
        }


    } // namespace code_generator
} // namespace rill
