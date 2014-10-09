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

// R: ( rule_name, ast type, rule )
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
                    return x3::lexeme[
                        x3::lit( std::forward<L>( literal ) ) >> +skip_grammer::rules::entrypoint()
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
                | t.class_definition_statement
                | t.empty_statement
                | t.expression_statement    // this rule must be located at last
            )



            // ========================================
            //
            R( function_definition_statement, ast::function_definition_statement_ptr,
                ( detail::make_keyword( "def" )
                > t.identifier
                > t.parameter_variable_declaration_list
                > -t.type_specifier
                > t.function_body_block
                )[
                    helper::make_node_ptr<ast::function_definition_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3,
                        ph::_4
                        )
                    ]
                )


            R( function_body_block, ast::statements_ptr,
                ( x3::lit( "{" ) >> t.program_body_statements >> x3::lit( "}" ) )[
                    helper::assign()
                    ]
                | ( x3::lit( "=>" ) >> t.expression >> t.statement_termination )[
                    helper::fun(
                        []( auto&&... args ) {
                            return std::make_shared<ast::statements>(
                                std::make_shared<ast::block_statement>(
                                    std::make_shared<ast::return_statement>(
                                        std::forward<decltype(args)>( args )...
                                        )
                                    )
                                );
                        },
                        ph::_1
                        )
                    ]
            )


            // executable scope, such as function, block, lambda, ...
            R( program_body_statement, ast::statement_ptr,
                ( t.empty_statement
                | t.expression_statement    // NOTE: this statement must be set at last
                )
            )

            R( program_body_statements, ast::statements_ptr,
                ( *t.program_body_statement )[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )


            R( holder_kind_specifier, attribute::quality_kind,
                ( x3::lit( "val" )[([]( auto& ctx ){ x3::_val( ctx ) = attribute::quality_kind::k_val; })]
                | x3::lit( "ref" )[([]( auto& ctx ){ x3::_val( ctx ) = attribute::quality_kind::k_ref; })]
                )
            )

            R( parameter_variable_declaration, ast::variable_declaration,
                t.holder_kind_specifier > t.parameter_variable_initializer_unit
            )

            R( parameter_variable_initializer_unit, ast::variable_declaration_unit,
               -t.identifier > t.value_initializer_unit
            )

            // value initializer unit
            // Ex.
            /// = 5
            /// = 5 :int
            /// :int
            R( value_initializer_unit, ast::value_initializer_unit,
                ( ( x3::lit( '=' ) > t.expression ) >> -t.type_specifier )[
                    helper::construct<ast::value_initializer_unit>( ph::_1, ph::_2 )
                    ]
                | t.type_specifier[
                    helper::construct<ast::value_initializer_unit>( ph::_1 )
                    ]
            )

            R( type_specifier, ast::type_expression_ptr,
                ( x3::lit( ':' ) > t.type_expression )
            )

            R( type_expression, ast::type_expression_ptr,
                t.id_expression[
                    helper::make_node_ptr<ast::type_expression>( ph::_1 )
                    ]
            )

            R( parameter_variable_declaration_list, ast::parameter_list,
                ( ( x3::lit( '(' ) >> x3::lit( ')' ) )
                | ( x3::lit( '(' ) >> ( t.parameter_variable_declaration % x3::lit( ',' ) ) >> x3::lit( ')' ) )
                )
            )



            //
            R( class_definition_statement, ast::class_definition_statement_ptr,
                ( detail::make_keyword( "class" )
                > t.identifier
                > t.class_body_block
                )[
                    helper::make_node_ptr<ast::class_definition_statement>(
                        ph::_1,
                        ph::_2
                        )
                    ]
            )


            R( class_body_block, ast::statements_ptr,
                ( x3::lit( "{" ) >> t.class_body_statements >> x3::lit( "}" ) )[
                    helper::assign()
                    ]
            )


            // executable scope, such as function, block, lambda, ...
            R( class_body_statement, ast::statement_ptr,
                ( t.empty_statement
                )
            )

            R( class_body_statements, ast::statements_ptr,
                ( *t.class_body_statement )[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
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
                t.identifier_normal | t.identifier_with_root
                )

            R( identifier_normal, ast::identifier_value_ptr,
                t.identifier_sequence[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, false )
                    ]
                )

            R( identifier_with_root, ast::identifier_value_ptr,
                ( x3::lit( '.' ) >> t.identifier_sequence )[
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
