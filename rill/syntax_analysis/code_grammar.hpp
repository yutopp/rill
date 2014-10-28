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

            // ====================================================================================================
            // code grammar
            // ====================================================================================================
            RILL_RULES_BEGIN( rules, program )

            // ====================================================================================================
            RC( program, ast::statements_ptr, ( on_error_annotator_base ),
                ( t.top_level_statements > ( x3::eol | x3::eoi ) )
                )


            // ====================================================================================================
            R( top_level_statements, ast::statements_ptr,
                ( *t.top_level_statement )[helper::make_node_ptr<ast::statements>( ph::_1 )]
                )

            R( top_level_statement, ast::statement_ptr,
                  t.function_definition_statement
                | t.class_definition_statement
                | t.extern_statement
                | t.template_statement
                | t.empty_statement
                | t.expression_statement    // this rule must be located at last
            )


            // ====================================================================================================
            // ====================================================================================================
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


            // ====================================================================================================
            // executable scope, such as function, block, lambda, ...
            R( program_body_statement, ast::statement_ptr,
                ( t.variable_declaration_statement
                | t.return_statement
                | t.empty_statement
                | t.expression_statement    // NOTE: this statement must be set at last
                )
            )

            R( program_body_statements, ast::statements_ptr,
                ( *t.program_body_statement )[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            R( parameter_variable_holder_kind_specifier, attribute::holder_kind,
                ( detail::make_keyword( "val" )[helper::assign(attribute::holder_kind::k_val)]
                | detail::make_keyword( "ref" )[helper::assign(attribute::holder_kind::k_ref)]
                )
            )

            R( parameter_variable_declaration, ast::variable_declaration,
                ( t.parameter_variable_holder_kind_specifier > t.parameter_variable_initializer_unit )[
                    helper::construct<ast::variable_declaration>( ph::_1, ph::_2 )
                    ]
            )

            R( parameter_variable_initializer_unit, ast::variable_declaration_unit,
                ( -t.identifier > t.value_initializer_unit )[
                    helper::construct<ast::variable_declaration_unit>( ph::_1, ph::_2 )
                    ]
            )

            R( parameter_variable_declaration_list, ast::parameter_list,
                ( ( x3::lit( '(' ) >> x3::lit( ')' ) )
                | ( x3::lit( '(' ) >> ( t.parameter_variable_declaration % x3::lit( ',' ) ) >> x3::lit( ')' ) )
                )
            )

            // value initializer unit
            // Ex.
            /// :int = 5
            /// = 5
            /// :int
            R( value_initializer_unit, ast::value_initializer_unit,
                ( x3::lit( '=' ) > t.expression )[
                    helper::construct<ast::value_initializer_unit>( ph::_1 )
                    ]
                | ( t.type_specifier >> -( x3::lit( '=' ) > t.expression ) )[
                    helper::construct<ast::value_initializer_unit>( ph::_1, ph::_2 )
                    ]
            )


            // ====================================================================================================
            R( type_specifier, ast::id_expression_ptr,
                ( x3::lit( ':' ) > t.id_expression )
            )


            // ====================================================================================================
            // ====================================================================================================
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


            // ====================================================================================================
            // executable scope, such as function, block, lambda, ...
            R( class_body_statement, ast::statement_ptr,
                ( t.class_function_definition_statement
                | t.class_variable_declaration_statement
                | t.empty_statement
                )
            )

            R( class_body_statements, ast::statements_ptr,
                ( *t.class_body_statement )[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )

            R( class_function_definition_statement, ast::class_function_definition_statement_ptr,
                ( detail::make_keyword( "def" )
                > t.identifier
                > t.parameter_variable_declaration_list
                > -t.type_specifier
                > t.function_body_block
                )[
                    helper::make_node_ptr<ast::class_function_definition_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3,
                        ph::_4
                        )
                    ]
            )

            R( class_variable_declaration_statement, ast::class_variable_declaration_statement_ptr,
                ( t.variable_declaration > t.statement_termination )[
                    helper::make_node_ptr<ast::class_variable_declaration_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            //
            R( extern_statement, ast::extern_statement_base_ptr,
                ( detail::make_keyword( "extern" )
                > ( t.extern_function_declaration_statement
                  )
                > t.statement_termination
                )
            )

            R( extern_function_declaration_statement, ast::extern_function_declaration_statement_ptr,
                ( detail::make_keyword( "def" )
                > t.identifier
                > t.parameter_variable_declaration_list
                > t.type_specifier
                > t.string_literal_sequence
                )[
                    helper::make_node_ptr<ast::extern_function_declaration_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3,
                        ph::_4
                        )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            //
            R( templatable_statement, ast::can_be_template_statement_ptr,
                ( t.function_definition_statement
                | t.class_definition_statement
                )
            )

            R( template_statement, ast::template_statement_ptr,
                ( x3::lit( "template" )
                > t.template_parameter_variable_declaration_list
                > t.templatable_statement
                )[
                    helper::make_node_ptr<ast::template_statement>(
                        ph::_1,
                        ph::_2
                        )
                    ]
            )

            R( template_parameter_variable_declaration, ast::variable_declaration,
                ( t.parameter_variable_initializer_unit )[
                    helper::construct<ast::variable_declaration>( attribute::holder_kind::k_ref, ph::_2 )
                    ]
            )

            R( template_parameter_variable_declaration_list, ast::parameter_list,
                ( ( x3::lit( '(' ) >> x3::lit( ')' ) )
                | ( x3::lit( '(' ) >> ( t.template_parameter_variable_declaration % x3::lit( ',' ) ) >> x3::lit( ')' ) )
                )
            )


            // ====================================================================================================
            // ====================================================================================================
            R( variable_declaration_statement, ast::variable_declaration_statement_ptr,
                ( t.variable_declaration > t.statement_termination )[
                    helper::make_node_ptr<ast::variable_declaration_statement>( ph::_1 )
                    ]
            )

            R( variable_holder_kind_specifier, attribute::holder_kind,
                ( detail::make_keyword( "val" )[helper::assign( attribute::holder_kind::k_val )]
                | detail::make_keyword( "ref" )[helper::assign( attribute::holder_kind::k_ref )]
                )
            )

            R( variable_declaration, ast::variable_declaration,
                ( t.variable_holder_kind_specifier > t.variable_initializer_unit )[
                    helper::construct<ast::variable_declaration>( ph::_1, ph::_2 )
                   ]
            )

            R( variable_initializer_unit, ast::variable_declaration_unit,
                ( t.identifier > t.value_initializer_unit )[
                    helper::construct<ast::variable_declaration_unit>( ph::_1, ph::_2 )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            R( expression_statement, ast::expression_statement_ptr,
                ( t.expression > t.statement_termination )[
                    helper::make_node_ptr<ast::expression_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            R( empty_statement, ast::empty_statement_ptr,
                t.statement_termination[
                    helper::make_node_ptr<ast::empty_statement>()
                    ]
                )


            // ====================================================================================================
            // ====================================================================================================
            R( return_statement, ast::return_statement_ptr,
                ( detail::make_keyword( "return" )
                > t.expression > t.statement_termination
                )[
                    helper::make_node_ptr<ast::return_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            //
            R( statement_termination, x3::unused_type,
                x3::lit( ';' )
                )


            // ====================================================================================================
            // TODO: make id_expression
            R( id_expression, ast::id_expression_ptr,
                ( t.conditional_expression
                )[
                    helper::fun(
                        []( auto&&... args ) {
                            return ast::helper::make_id_expression( std::forward<decltype(args)>( args )... );
                        },
                        ph::_1
                        )
                    ]
            )


            // ====================================================================================================
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
                >> *(
                        ( x3::lit( '.' ) >> t.identifier_value_set )[
                            helper::make_assoc_node_ptr<ast::element_selector_expression>( ph::_1 )
                            ]
                        | ( x3::lit( '[' ) > -t.expression > x3::lit( ']' ) )[
                            helper::make_assoc_node_ptr<ast::subscrpting_expression>( ph::_1 )
                            ]
                        | ( t.argument_list )[
                            helper::make_assoc_node_ptr<ast::call_expression>( ph::_1 )
                            ]
                    )
            )

            R( primary_expression, ast::expression_ptr,
                t.primary_value[helper::make_node_ptr<ast::term_expression>( ph::_1 )]
                | ( x3::lit( '(' ) >> t.expression >> x3::lit( ')' ) )[helper::assign()]
            )


            R( argument_list, ast::expression_list,
                ( x3::lit( '(' ) >> x3::lit( ')' ) )
                | ( x3::lit( '(' ) >> ( t.assign_expression % ',' ) >> x3::lit( ')' ) )
            )


            // ====================================================================================================
            // ====================================================================================================
            R( primary_value, ast::value_ptr,
                ( t.identifier_value_set
                | t.numeric_literal
                | t.boolean_literal
                | t.string_literal
                | t.array_literal
                )
            )


            // ====================================================================================================
            R( identifier_value_set, ast:: identifier_value_base_ptr,
                t.template_instance_identifier | t.identifier
            )


            R( identifier, ast::identifier_value_ptr,
                t.identifier_from_root | t.identifier_relative
            )

            R( identifier_relative, ast::identifier_value_ptr,
                t.identifier_sequence[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, false )
                    ]
            )

            R( identifier_from_root, ast::identifier_value_ptr,
                ( x3::lit( '.' ) >> t.identifier_sequence )[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, true )
                    ]
            )


            R( template_instance_identifier, ast::template_instance_value_ptr,
                t.template_instance_identifier_from_root | t.template_instance_identifier_relative
            )

            R( template_instance_identifier_relative, ast::template_instance_value_ptr,
                ( t.identifier_sequence >> x3::lit( '!' ) >> t.argument_list )[
                    helper::make_node_ptr<ast::template_instance_value>( ph::_1, ph::_2, false )
                    ]
            )

            R( template_instance_identifier_from_root, ast::template_instance_value_ptr,
                ( x3::lit( '.' ) >> t.identifier_sequence >> x3::lit( '!' ) >> t.argument_list )[
                    helper::make_node_ptr<ast::template_instance_value>( ph::_1, ph::_2, true )
                    ]
            )

            // ====================================================================================================
            R( numeric_literal, ast::intrinsic::int32_value_ptr/*TODO: change*/,
                t.integer_literal
            )

            R( integer_literal, ast::intrinsic::int32_value_ptr,
                x3::int_[
                    helper::make_node_ptr<ast::intrinsic::int32_value>( ph::_1 )
                    ]
            )

            // ====================================================================================================
            R( boolean_literal, ast::intrinsic::boolean_value_ptr,
                  x3::lit( "true" )[helper::make_node_ptr<ast::intrinsic::boolean_value>( true )]
                | x3::lit( "false" )[helper::make_node_ptr<ast::intrinsic::boolean_value>( false )]
            )

            // ====================================================================================================
            R( array_literal, ast::intrinsic::array_value_ptr,
                ( ( x3::lit( '[' ) >> x3::lit( ']' ) )[
                    helper::make_node_ptr<ast::intrinsic::array_value>()
                    ] )
                | ( ( x3::lit( '[' ) >> ( t.assign_expression % ',' ) >> x3::lit( ']' ) )[
                        helper::make_node_ptr<ast::intrinsic::array_value>( ph::_1 )
                        ] )
            )

            // ====================================================================================================
            R( string_literal, ast::intrinsic::string_value_ptr,
                t.string_literal_sequence[
                    helper::make_node_ptr<ast::intrinsic::string_value>( ph::_1 )
                    ]
            )

            R( string_literal_sequence, std::string,
                x3::lexeme[
                    x3::lit( '"' ) >> *( ( t.escape_sequence | x3::char_ ) - x3::lit( '"' ) ) >> x3::lit( '"' )
                    ]
            )

            // TODO: support some escape sequences
            R( escape_sequence, char,
                x3::lit( "\\n" )[helper::construct<char>( '\n' )]
            )


            // ====================================================================================================
            // ====================================================================================================
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
                | x3::char_( '_' )
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
