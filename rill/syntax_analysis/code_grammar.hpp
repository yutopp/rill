//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_CODE_GRAMMAR_HPP
#define RILL_SYNTAX_ANALYSIS_CODE_GRAMMAR_HPP

#include <boost/spirit/home/x3.hpp>

#include "../ast.hpp"
#include "skip_grammar.hpp"
#include "range_workaround.hpp"
#include "helper.hpp"


#define R   RILL_RULE
#define RC  RILL_RULE_WITH_ANNOTATOR

namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        using range = x3::range_char<boost::spirit::char_encoding::standard>;

        namespace code_grammar
        {
            namespace ph = placeholders;

            namespace detail
            {
                template<typename L>
                auto make_keyword( L&& literal )
                {
                    return x3::no_skip[
                        x3::lit( std::forward<L>( literal ) )
                        >> +skip_grammer::rules::entrypoint()
                    ];
                }
            } // namespace detail

            //
            RILL_RULES_BEGIN( rules, program )
            // ========================================
            RC( program, ast::statements_ptr, ( on_error_annotator_base ),
                ( t.top_level_statements > ( x3::eol | x3::eoi ) )
                )

            // ========================================
            R( top_level_statements, ast::statements_ptr,
                ( *t.top_level_statement )[helper::make_node_ptr<ast::statements>( ph::_1 )]
                )

            R( top_level_statement, ast::statement_ptr,
                t.function_definition_statement
                | t.empty_statement
                | t.expression_statement /*this rule must be located at last*/
                )



            // ========================================
            //
            R( function_definition_statement, ast::function_definition_statement_ptr,
                ( detail::make_keyword( "def" )
                > t.id_expression
                )[
                    helper::make_node_ptr<ast::function_definition_statement>(
                        nullptr,
                        ast::parameter_list{},
                        boost::none,
                        nullptr
                        )
                    ]
                )

            //
            R( expression_statement, ast::expression_statement_ptr,
                ( t.expression > t.statement_termination )[
                    helper::make_node_ptr<ast::expression_statement>( ph::_1 )
                    ]
            )

            //
            R( empty_statement, ast::empty_statement_ptr,
                t.statement_termination[
                    helper::make_node_ptr<ast::empty_statement>()
                    ]
                )

            // ========================================
            //
            R( statement_termination, x3::unused_type,
                x3::lit( ';' )
                )

            // ========================================
            // TODO: make id_expression
            R( id_expression, ast::id_expression_ptr,
                ( t.identifier_value_set
                )[helper::make_node_ptr<ast::id_expression>( ph::_1 )]
            )

#if 0
            R( type_specifier, ast::element::type_spec,
                x3::lit( ':' )
                >> ( ( x3::lit( '.' ) >> t.qualifier_list )
                   | t.type_expression
                   )
                )

            R( type_expression, ast::type_expression_ptr,
                t.assign_expression
                )
#endif

            R( expression, ast::expression_ptr,
                t.assign_expression // NOT commma_expression
                )

            //
            R( commma_expression, ast::expression_ptr,
                t.assign_expression[helper::assign()]
                >> *( ( x3::lit( ',' ) >> t.assign_expression )[helper::make_left_assoc_binary_op_node_ptr( ",", ph::_1 )]
                   )
                )

            //
            R( assign_expression, ast::expression_ptr,
                t.conditional_expression[helper::assign()]
                >> *( ( x3::lit( "=" ) >> t.conditional_expression )[helper::make_left_assoc_binary_op_node_ptr( "=", ph::_1 )]
                    )
            )

            //
            R( conditional_expression, ast::expression_ptr,
                t.logical_or_expression[helper::assign()]
                // TODO: add conditional operator( ? : )
            )

            //
            R( logical_or_expression, ast::expression_ptr,
                t.logical_and_expression[helper::assign()]
                >> *( ( x3::lit( "||" ) >> t.logical_and_expression )[helper::make_left_assoc_binary_op_node_ptr( "||", ph::_1 )]
                    )
            )

            //
            R( logical_and_expression, ast::expression_ptr,
                t.bitwise_or_expression[helper::assign()]
                >> *( ( x3::lit( "&&" ) >> t.bitwise_or_expression )[helper::make_left_assoc_binary_op_node_ptr( "&&", ph::_1 )]
                    )
            )

            //
            R( bitwise_or_expression, ast::expression_ptr,
                t.bitwise_xor_expression[helper::assign()]
                >> *( ( x3::lit( "|" ) >> t.bitwise_xor_expression )[helper::make_left_assoc_binary_op_node_ptr( "|", ph::_1 )]
                    )
            )

            //
            R( bitwise_xor_expression, ast::expression_ptr,
                t.bitwise_and_expression[helper::assign()]
                >> *( ( x3::lit( "^" ) >> t.bitwise_and_expression )[helper::make_left_assoc_binary_op_node_ptr( "^", ph::_1 )]
                    )
            )

            //
            R( bitwise_and_expression, ast::expression_ptr,
                t.equality_expression[helper::assign()]
                >> *( ( x3::lit( "&" ) >> t.equality_expression )[helper::make_left_assoc_binary_op_node_ptr( "&", ph::_1 )]
                    )
            )

            //
            R( equality_expression, ast::expression_ptr,
                t.relational_expression[helper::assign()]
                >> *( ( x3::lit( "==" ) >> t.relational_expression )[helper::make_left_assoc_binary_op_node_ptr( "==", ph::_1 )]
                    | ( x3::lit( "!=" ) >> t.relational_expression )[helper::make_left_assoc_binary_op_node_ptr( "!=", ph::_1 )]
                    )
            )

            //
            R( relational_expression, ast::expression_ptr,
                t.shift_expression[helper::assign()]
                >> *( ( x3::lit( "<=" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( "<=", ph::_1 )]
                    | ( x3::lit( "<" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( "<", ph::_1 )]
                    | ( x3::lit( ">=" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( ">=", ph::_1 )]
                    | ( x3::lit( ">" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( ">", ph::_1 )]
                    )
            )

            //
            R( shift_expression, ast::expression_ptr,
                t.add_sub_expression[helper::assign()]
                >> *( ( x3::lit( "<<" ) >> t.add_sub_expression )[helper::make_left_assoc_binary_op_node_ptr( "<<", ph::_1 )]
                    | ( x3::lit( ">>" ) >> t.add_sub_expression )[helper::make_left_assoc_binary_op_node_ptr( ">>", ph::_1 )]
                    )
            )

            //
            R( add_sub_expression, ast::expression_ptr,
                t.mul_div_rem_expression[helper::assign()]
                >> *( ( x3::lit( "+" ) >> t.mul_div_rem_expression )[helper::make_left_assoc_binary_op_node_ptr( "+", ph::_1 )]
                    | ( x3::lit( "-" ) >> t.mul_div_rem_expression )[helper::make_left_assoc_binary_op_node_ptr( "-", ph::_1 )]
                    )
            )


            //
            R( mul_div_rem_expression, ast::expression_ptr,
                t.unary_expression[helper::assign()]
                >> *( ( x3::lit( "*" ) >> t.unary_expression )[helper::make_left_assoc_binary_op_node_ptr( "*", ph::_1 )]
                    | ( x3::lit( "/" ) >> t.unary_expression )[helper::make_left_assoc_binary_op_node_ptr( "/", ph::_1 )]
                    | ( x3::lit( "%" ) >> t.unary_expression )[helper::make_left_assoc_binary_op_node_ptr( "%", ph::_1 )]
                    )
            )

            //
            R( unary_expression, ast::expression_ptr,
                t.postfix_expression[helper::assign()]
                // TODO: add unary operator( + - )
            )

            //
            R( postfix_expression, ast::expression_ptr,
                t.primary_expression[helper::assign()]
                >> *( ( x3::lit( '.' ) >> t.identifier_value_set )[
                          helper::make_assoc_node_ptr<ast::element_selector_expression>( ph::_1 )
                          ]
                    | ( x3::lit( '[' ) > -t.expression > x3::lit( ']' ) )[
                        helper::make_assoc_node_ptr<ast::subscrpting_expression>( ph::_1 )
                        ]
                    )
            )

            R( primary_expression, ast::expression_ptr,
                t.primary_value[helper::make_node_ptr<ast::term_expression>( ph::_1 )]
                | ( x3::lit( '(' ) >> t.expression >> x3::lit( ')' ) )[helper::assign()]
            )

            R( primary_value, ast::value_ptr,
                ( t.identifier_with_root
                | t.identifier
                )
            )

            R( identifier_value_set, ast:: identifier_value_base_ptr,
                ( t.identifier_with_root
                | t.identifier
                )
            )

            R( identifier, ast::identifier_value_ptr,
                t.identifier_sequence[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, false )
                    ]
                )

            R( identifier_with_root, ast::identifier_value_ptr,
                ( x3::lit( '.' )
                >> t.identifier_sequence
                )[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, true )
                    ]
                )

#if 0
template_instance_value
            auto const template_instance_def
                = ( identifier_sequence >> x3::lit( '!' ) /*>> argument_list_*/ )/*[
                                                                                   qi::_val = helper::make_node_ptr<ast::template_instance_value>( qi::_1, qi::_2 )
                                                                                   ]*/
                ;


            auto template_instance_with_root_
                = ( x3::lit( '.' )
                >> identifier_sequence >> x3::lit( '!' ) /*>> argument_list_*/ )/*[
                       qi::_val = helper::make_node_ptr<ast::template_instance_value>( qi::_1, qi::_2, phx::val( true ) )
                       ]*/
                ;
#endif

            R( identifier_sequence, std::string,
                x3::lexeme[
                    ( t.nondigit_charset )
                    >> *( t.nondigit_charset
                        | t.digit_charset
                        )
                ]
                )

            R( nondigit_charset, char,
                range( 'A', 'Z' )
                | range( 'a', 'z' )
                | '_'
                )

            R( digit_charset, char,
                range( '0', '9' )
                )

            RILL_RULES_END

        } // namespace code_grammar
    } // namespace syntax_analysis
} // namespace rill

#undef R
#undef RC

#endif /*RILL_SYNTAX_ANALYSIS_CODE_GRAMMAR_HPP*/
