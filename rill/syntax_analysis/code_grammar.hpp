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
// #include <boost/fusion/include/adapt_struct.hpp>

#include "../ast.hpp"
#include "skip_grammar.hpp"
#include "range_workaround.hpp"
#include "helper.hpp"

// R: ( rule_name, ast type, rule )
#define R                                       \
    RILL_RULE

#define RA                                      \
    RILL_RULE_WITH_ANNOTATOR

#define RN( name, type, ... )                               \
    RILL_RULE( name, type, tagged( __VA_ARGS__ ) )


namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        using range = x3::range_char<boost::spirit::char_encoding::standard>;

        namespace code_grammar
        {
            namespace ph = placeholders;

            template<typename L>
            auto make_keyword( L&& literal )
            {
                return x3::lexeme[
                    x3::lit( std::forward<L>( literal ) )
                    >> !( range( 'A', 'Z' )
                        | range( 'a', 'z' )
                        | x3::char_( '_' )
                        | range( '0', '9' )
                        )
                    ];
            }

            template<typename L>
            decltype(auto) tagged( L&& rule )
            {
                return x3::raw[rule][helper::tagging()];
            }

            // ====================================================================================================
            // code grammar
            // ====================================================================================================
            RILL_RULES_BEGIN( rules, program )

            // ====================================================================================================
            RA( program, ast::module_ptr,
                ( on_error_annotator_base ),
                t.module > ( x3::eol | x3::eoi )
            )

            RN( module, ast::module_ptr,
                t.top_level_statements[
                    helper::make_node_ptr<ast::module>( nullptr, ph::_1 )
                    ]
            )

            // ====================================================================================================
            RN( top_level_statements, ast::statements_ptr,
                ( *t.top_level_statement )[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )

            R( top_level_statement, ast::statement_ptr,
                  t.function_definition_statement
                | t.class_definition_statement
                | t.extern_statement
                | t.template_statement
                | t.import_statement
                | t.empty_statement
                | t.expression_statement    // this rule must be located at last
            )


            // ====================================================================================================
            // ====================================================================================================
            //
            RN( function_definition_statement, ast::function_definition_statement_ptr,
                ( make_keyword( "def" )
                > t.identifier
                > t.parameter_variable_declaration_list
                > t.decl_attribute_list
                > -t.type_specifier
                > t.function_body_block
                )[
                    helper::make_node_ptr<ast::function_definition_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3,
                        ph::_4,
                        ph::_5
                        )
                    ]
            )

            R( function_body_statements_list, ast::element::statement_list,
                ( x3::lit( "{" ) >> t.program_body_statements_list >> x3::lit( "}" ) )
                | t.function_online_body_for_normal
            )

            R( function_body_statements_list_for_lambda, ast::element::statement_list,
                ( x3::lit( "{" ) >> t.program_body_statements_list >> x3::lit( "}" ) )
                | t.function_online_body_for_lambda
            )

            R( function_online_body_for_normal, ast::element::statement_list,
                ( x3::lit( "=>" ) >> t.expression >> t.statement_termination )[
                    helper::fun(
                        []( auto&&... args ) {
                            return ast::element::statement_list{
                                std::make_shared<ast::block_statement>(
                                    std::make_shared<ast::return_statement>(
                                        std::forward<decltype(args)>( args )...
                                        )
                                    )
                                };
                        },
                        ph::_1
                        )
                    ]
            )

            R( function_online_body_for_lambda, ast::element::statement_list,
                ( x3::lit( "=>" ) >> t.expression )[
                    helper::fun(
                        []( auto&&... args ) {
                            return ast::element::statement_list{
                                std::make_shared<ast::block_statement>(
                                    std::make_shared<ast::return_statement>(
                                        std::forward<decltype(args)>( args )...
                                        )
                                    )
                                };
                        },
                        ph::_1
                        )
                    ]
            )

            RN( function_body_block, ast::statements_ptr,
                t.function_body_statements_list[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            // executable scope, such as function, block, lambda, ...
            R( program_body_statement, ast::statement_ptr,
                ( t.block_statement
                | t.variable_declaration_statement
                | t.control_flow_statement
                | t.return_statement
                | t.empty_statement
                | t.expression_statement    // NOTE: this statement must be set at last
                )
            )

            R( program_body_statements_list, ast::element::statement_list,
                *t.program_body_statement
            )

            RN( program_body_statements, ast::statements_ptr,
                t.program_body_statements_list[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )

            RN( block_statement, ast::block_statement_ptr,
                ( x3::lit( "{" ) >> t.program_body_statements >> x3::lit( "}" ) )[
                    helper::make_node_ptr<ast::block_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            R( parameter_variable_holder_kind_specifier, attribute::holder_kind,
                ( make_keyword( "val" )[helper::assign( attribute::holder_kind::k_val )]
                | make_keyword( "ref" )[helper::assign( attribute::holder_kind::k_ref )]
                | x3::eps[helper::assign( attribute::holder_kind::k_ref )]
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

            R( decl_attribute, attribute::decl::type,
                ( x3::lit( "onlymeta" )[helper::assign( attribute::decl::k_onlymeta )]
                | x3::lit( "intrinsic" )[helper::assign( attribute::decl::k_intrinsic )]
                )
            )

            R( decl_attribute_list, attribute::decl::type,
                x3::attr( attribute::decl::k_default )
                >> ( ( t.decl_attribute[helper::make_merged_bitflag( ph::_1 )] % x3::lit( ',' ) )
                   | x3::eps
                   )
            )

            // ====================================================================================================
            // ====================================================================================================
            RN( class_definition_statement, ast::class_definition_statement_ptr,
                ( make_keyword( "class" )
                > t.identifier
                > t.decl_attribute_list
                > t.class_body_block
                )[
                    helper::make_node_ptr<ast::class_definition_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3
                        )
                    ]
            )

            R( class_body_block, ast::statements_ptr,
                x3::lit( "{" ) >> t.class_body_statements >> x3::lit( "}" )
            )


            // ====================================================================================================
            //
            R( class_body_statement, ast::statement_ptr,
                ( t.class_function_definition_statement
                | t.class_variable_declaration_statement
                | t.class_template_statement
                | t.empty_statement
                )
            )

            RN( class_body_statements, ast::statements_ptr,
                ( *t.class_body_statement )[
                    helper::make_node_ptr<ast::statements>( ph::_1 )
                    ]
            )


            R( class_templatable_statement, ast::can_be_template_statement_ptr,
                ( t.class_function_definition_statement
                )
            )

            RN( class_template_statement, ast::template_statement_ptr,
                ( x3::lit( "template" )
                > t.template_parameter_variable_declaration_list
                > t.class_templatable_statement
                )[
                    helper::make_node_ptr<ast::template_statement>(
                        ph::_1,
                        ph::_2
                        )
                    ]
            )


            RN( class_function_definition_statement, ast::class_function_definition_statement_ptr,
                ( make_keyword( "def" )
                > t.identifier
                > t.parameter_variable_declaration_list
                > t.decl_attribute_list
                > -t.type_specifier
                > t.function_body_block
                )[
                    helper::make_node_ptr<ast::class_function_definition_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3,
                        ph::_4,
                        ph::_5
                        )
                    ]
            )

            RN( class_variable_declaration_statement, ast::class_variable_declaration_statement_ptr,
                ( t.variable_declaration > t.statement_termination )[
                    helper::make_node_ptr<ast::class_variable_declaration_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            //
            R( extern_statement, ast::can_be_template_statement_ptr,
                ( make_keyword( "extern" )
                > ( t.extern_function_declaration_statement
                  | t.extern_class_declaration_statement
                  )
                > t.statement_termination
                )
            )

            RN( extern_function_declaration_statement, ast::extern_function_declaration_statement_ptr,
                ( make_keyword( "def" )
                > t.identifier
                > t.parameter_variable_declaration_list
                > t.extern_decl_attribute_list
                > t.type_specifier
                > t.string_literal_sequence
                )[
                    helper::make_node_ptr<ast::extern_function_declaration_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3,
                        ph::_4,
                        ph::_5
                        )
                    ]
            )

            RN( extern_class_declaration_statement, ast::extern_class_declaration_statement_ptr,
                ( make_keyword( "class" )
                > t.identifier
                > t.extern_decl_attribute_list
                > t.string_literal_sequence
                )[
                    helper::make_node_ptr<ast::extern_class_declaration_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3
                        )
                    ]
            )

            R( extern_decl_attribute_list, attribute::decl::type,
               t.decl_attribute_list[helper::assign()] >> x3::eps[helper::make_merged_bitflag( attribute::decl::k_extern )]
            )


            // ====================================================================================================
            // ====================================================================================================
            //
            R( templatable_statement, ast::can_be_template_statement_ptr,
                ( t.function_definition_statement
                | t.class_definition_statement
                | t.extern_statement
                )
            )

            RN( template_statement, ast::template_statement_ptr,
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
                    helper::construct<ast::variable_declaration>( attribute::holder_kind::k_ref, ph::_1 )
                    ]
            )

            R( template_parameter_variable_declaration_list, ast::parameter_list,
                ( ( x3::lit( '(' ) >> x3::lit( ')' ) )
                | ( x3::lit( '(' ) >> ( t.template_parameter_variable_declaration % x3::lit( ',' ) ) >> x3::lit( ')' ) )
                )
            )


            // ====================================================================================================
            // ====================================================================================================
            RN( variable_declaration_statement, ast::variable_declaration_statement_ptr,
                ( t.variable_declaration > t.statement_termination )[
                    helper::make_node_ptr<ast::variable_declaration_statement>( ph::_1 )
                    ]
            )

            R( variable_holder_kind_specifier, attribute::holder_kind,
                ( make_keyword( "val" )[helper::assign( attribute::holder_kind::k_val )]
                | make_keyword( "ref" )[helper::assign( attribute::holder_kind::k_ref )]
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
            RN( import_statement, ast::import_statement_ptr,
                ( make_keyword( "import" )
                > x3::attr(nullptr) /* work around to avoid this rule to be adapted to vector(pass type at random) */
                > t.import_decl_unit_list
                > t.statement_termination
                )[
                    helper::make_node_ptr<ast::import_statement>( ph::_2 )
                    ]
            )

            R( import_decl_unit, ast::import_decl_unit,
                t.normal_identifier_sequence[
                    helper::construct<ast::import_decl_unit>( ph::_1 )
                    ]
            )

            R( import_decl_unit_list, ast::import_decl_unit_list,
                t.import_decl_unit % x3::lit( ',' )
            )


            // ====================================================================================================
            // ====================================================================================================
            R( control_flow_statement, ast::statement_ptr,
                ( t.while_statement
                | t.if_statement
                )
            )


            RN( while_statement, ast::while_statement_ptr,
                ( x3::lit( "while" )
                > ( x3::lit( "(" ) > t.expression > x3::lit( ")" ) )
                > t.program_body_statement
                )[
                    helper::make_node_ptr<ast::while_statement>(
                        ph::_1,
                        ph::_2
                        )
                    ]
            )


            RN( if_statement, ast::if_statement_ptr,
                ( x3::lit( "if" )
                > ( x3::lit( "(" ) > t.expression > x3::lit( ")" ) )
                > t.program_body_statement
                > -( x3::lit( "else" ) > t.program_body_statement )
                )[
                    helper::make_node_ptr<ast::if_statement>(
                        ph::_1,
                        ph::_2,
                        ph::_3
                        )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            RN( empty_statement, ast::empty_statement_ptr,
                t.statement_termination[
                    helper::make_node_ptr<ast::empty_statement>()
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            RN( return_statement, ast::return_statement_ptr,
                ( make_keyword( "return" )
                > t.expression > t.statement_termination
                )[
                    helper::make_node_ptr<ast::return_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            // ====================================================================================================
            RN( expression_statement, ast::expression_statement_ptr,
                ( t.expression > t.statement_termination )[
                    helper::make_node_ptr<ast::expression_statement>( ph::_1 )
                    ]
            )


            // ====================================================================================================
            //
            R( statement_termination, x3::unused_type,
                x3::lit( ';' )
            )


            // ====================================================================================================
            // TODO: make id_expression
            RN( id_expression, ast::id_expression_ptr,
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
                >> *tagged(
                    ( x3::lit( ',' ) >> t.assign_expression )[helper::make_left_assoc_binary_op_node_ptr( ",", ph::_1 )]
                    )
            )

            //
            RN( assign_expression, ast::expression_ptr,
               t.conditional_expression[helper::assign()]
                >> *( ( x3::lit( "=" ) >> t.conditional_expression )[helper::make_left_assoc_binary_op_node_ptr( "=", ph::_1 )]
                    )

            )

            //
            RN( conditional_expression, ast::expression_ptr,
                t.logical_or_expression[helper::assign()]
                // TODO: add conditional operator( ? : )
            )

            //
            R( logical_or_expression, ast::expression_ptr,
                t.logical_and_expression[helper::assign()]
                >> *tagged(
                    ( x3::lit( "||" ) > t.logical_and_expression )[helper::make_left_assoc_binary_op_node_ptr( "||", ph::_1 )]
                    )
            )

            //
            R( logical_and_expression, ast::expression_ptr,
                t.bitwise_or_expression[helper::assign()]
                >> *tagged(
                    ( x3::lit( "&&" ) >> t.bitwise_or_expression )[helper::make_left_assoc_binary_op_node_ptr( "&&", ph::_1 )]
                    )
            )

            //
            R( bitwise_or_expression, ast::expression_ptr,
                t.bitwise_xor_expression[helper::assign()]
                >> *tagged(
                    ( x3::lit( "|" ) >> t.bitwise_xor_expression )[helper::make_left_assoc_binary_op_node_ptr( "|", ph::_1 )]
                    )
            )

            //
            R( bitwise_xor_expression, ast::expression_ptr,
                t.bitwise_and_expression[helper::assign()]
                >> *tagged(
                    ( x3::lit( "^" ) >> t.bitwise_and_expression )[helper::make_left_assoc_binary_op_node_ptr( "^", ph::_1 )]
                    )
            )

            //
            R( bitwise_and_expression, ast::expression_ptr,
                t.equality_expression[helper::assign()]
                >> *tagged(
                    ( x3::lit( "&" ) >> t.equality_expression )[helper::make_left_assoc_binary_op_node_ptr( "&", ph::_1 )]
                    )
            )

            //
            R( equality_expression, ast::expression_ptr,
                t.relational_expression[helper::assign()]
                >> *tagged(
                      ( x3::lit( "==" ) >> t.relational_expression )[helper::make_left_assoc_binary_op_node_ptr( "==", ph::_1 )]
                    | ( x3::lit( "!=" ) >> t.relational_expression )[helper::make_left_assoc_binary_op_node_ptr( "!=", ph::_1 )]
                    )
            )

            //
            R( relational_expression, ast::expression_ptr,
                t.shift_expression[helper::assign()]
                >> *tagged(
                      ( x3::lit( "<=" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( "<=", ph::_1 )]
                    | ( x3::lit( "<" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( "<", ph::_1 )]
                    | ( x3::lit( ">=" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( ">=", ph::_1 )]
                    | ( x3::lit( ">" ) >> t.shift_expression )[helper::make_left_assoc_binary_op_node_ptr( ">", ph::_1 )]
                    )
            )

            //
            R( shift_expression, ast::expression_ptr,
                t.add_sub_expression[helper::assign()]
                >> *tagged(
                      ( x3::lit( "<<" ) >> t.add_sub_expression )[helper::make_left_assoc_binary_op_node_ptr( "<<", ph::_1 )]
                    | ( x3::lit( ">>" ) >> t.add_sub_expression )[helper::make_left_assoc_binary_op_node_ptr( ">>", ph::_1 )]
                    )
            )

            //
            R( add_sub_expression, ast::expression_ptr,
                t.mul_div_rem_expression[helper::assign()]
                >> *tagged(
                      ( x3::lit( "+" ) >> t.mul_div_rem_expression )[helper::make_left_assoc_binary_op_node_ptr( "+", ph::_1 )]
                    | ( x3::lit( "-" ) >> t.mul_div_rem_expression )[helper::make_left_assoc_binary_op_node_ptr( "-", ph::_1 )]
                    )
            )


            //
            R( mul_div_rem_expression, ast::expression_ptr,
                t.unary_expression[helper::assign()]
                >> *tagged(
                      ( x3::lit( "*" ) >> t.unary_expression )[helper::make_left_assoc_binary_op_node_ptr( "*", ph::_1 )]
                    | ( x3::lit( "/" ) >> t.unary_expression )[helper::make_left_assoc_binary_op_node_ptr( "/", ph::_1 )]
                    | ( x3::lit( "%" ) >> t.unary_expression )[helper::make_left_assoc_binary_op_node_ptr( "%", ph::_1 )]
                    )
            )

            //
            R( unary_expression, ast::expression_ptr,
                ( t.postfix_expression[helper::assign()]
                | tagged(
                    ( x3::lit( '-' ) >> t.unary_expression )[
                        helper::make_unary_prefix_op_node_ptr( "-", ph::_1 )
                        ])
                | tagged(
                    ( x3::lit( '+' ) >> t.unary_expression )[
                        helper::make_unary_prefix_op_node_ptr( "+", ph::_1 )
                        ])
                | tagged(
                    ( x3::lit( '*' ) >> t.unary_expression )[
                        helper::make_node_ptr<ast::dereference_expression>( ph::_1 )
                        ])
                | tagged(
                    ( x3::lit( '&' ) >> t.unary_expression )[
                        helper::make_node_ptr<ast::addressof_expression>( ph::_1 )
                        ])
                )
            )

            //
            R( postfix_expression, ast::expression_ptr,
                t.primary_expression[helper::assign()]
                >> *tagged(
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

            RN( primary_expression, ast::expression_ptr,
                ( t.primary_value[helper::make_node_ptr<ast::term_expression>( ph::_1 )]
                | ( x3::lit( '(' ) >> t.expression >> x3::lit( ')' ) )[helper::assign()]
                | t.lambda_expression[helper::assign()]
                )
            )


            R( argument_list, ast::expression_list,
                ( x3::lit( '(' ) >> x3::lit( ')' ) )
                | ( x3::lit( '(' ) >> ( t.assign_expression % ',' ) >> x3::lit( ')' ) )
            )


            // ====================================================================================================
            // ====================================================================================================
            RN( lambda_expression, ast::lambda_expression_ptr,
                ( t.lambda_introducer
                > t.parameter_variable_declaration_list
                > t.function_body_statements_list_for_lambda
                )[
                    helper::make_node_ptr<ast::lambda_expression>( ph::_1, ph::_2 )
                    ]
            )

            R( lambda_introducer, x3::unused_type,
                x3::lit( "->" )
            )

            /*
t.parameter_variable_declaration_list
                > t.decl_attribute_list
                > -t.type_specifier
                > t.function_body_block
            */

            // ====================================================================================================
            // ====================================================================================================
            R( primary_value, ast::value_ptr,
                ( t.boolean_literal
                | t.identifier_value_set
                | t.numeric_literal
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

            RN( identifier_relative, ast::identifier_value_ptr,
                t.identifier_sequence[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, false )
                    ]
            )

            RN( identifier_from_root, ast::identifier_value_ptr,
                ( x3::lit( '.' ) >> t.identifier_sequence )[
                    helper::make_node_ptr<ast::identifier_value>( ph::_1, true )
                    ]
            )


            R( template_instance_identifier, ast::template_instance_value_ptr,
                t.template_instance_identifier_from_root | t.template_instance_identifier_relative
            )

            RN( template_instance_identifier_relative, ast::template_instance_value_ptr,
                ( t.identifier_sequence >> t.template_argument_list )[
                    helper::make_node_ptr<ast::template_instance_value>( ph::_1, ph::_2, false )
                    ]
            )

            RN( template_instance_identifier_from_root, ast::template_instance_value_ptr,
                ( x3::lit( '.' ) >> t.identifier_sequence >> t.template_argument_list )[
                    helper::make_node_ptr<ast::template_instance_value>( ph::_1, ph::_2, true )
                    ]
            )

            R( template_argument_list, ast::expression_list,
                  ( x3::lit( '!' ) >> t.argument_list )[helper::assign()]
                | ( x3::lit( '!' ) >> t.primary_expression )[
                    helper::fun(
                        []( auto&&... args ) {
                            return ast::expression_list{ std::forward<decltype(args)>( args )... };
                        },
                        ph::_1
                        )
                    ]
            )

            // ====================================================================================================
            R( numeric_literal, ast::value_ptr,
                ( t.float_literal
                | t.integer_literal
                )
            )

            RN( integer_literal, ast::intrinsic::int32_value_ptr,
                x3::uint_[
                    helper::make_node_ptr<ast::intrinsic::int32_value>( ph::_1 )
                    ]
            )

            // TODO: check range
            RN( float_literal, ast::intrinsic::float_value_ptr,
                t.fp_[
                    helper::make_node_ptr<ast::intrinsic::float_value>( ph::_1 )
                    ]
            )

            // 1.0
            // 1.e0
            struct very_strict_fp_policies
                : public x3::strict_ureal_policies<long double>
            {
                static bool const allow_leading_dot = false;
                static bool const allow_trailing_dot = false;
            };
            x3::real_parser<long double, very_strict_fp_policies> const fp_;

#if 0
            // TODO:
            fp <-
            ( fractional_constant >> -exponent_part>> -floating_suffix )
            | ( +digit_charset >> exponent_part >> -floating_suffix )

            fractional_constant <-
                +digit_charset >> x3::lit( '.' ) >> +digit_charset

            sign <-
                lit('+') | lit('-')

            exponent_part <-
                (lit('e') | 'E') >> -sign >> +digit_charset

            floating_suffix <-
                lit('f') | 'l' | 'F' | 'L'
#endif


            // ====================================================================================================
            RN( boolean_literal, ast::intrinsic::boolean_value_ptr,
                x3::bool_[
                    helper::make_node_ptr<ast::intrinsic::boolean_value>( ph::_1 )
                    ]
            )

            // ====================================================================================================
            RN( array_literal, ast::intrinsic::array_value_ptr,
                ( ( x3::lit( '[' ) >> x3::lit( ']' ) )[
                    helper::make_node_ptr<ast::intrinsic::array_value>()
                    ] )
                | ( ( x3::lit( '[' ) >> ( t.assign_expression % ',' ) >> x3::lit( ']' ) )[
                        helper::make_node_ptr<ast::intrinsic::array_value>( ph::_1 )
                        ] )
            )

            // ====================================================================================================
            RN( string_literal, ast::intrinsic::string_value_ptr,
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
                ( t.operator_identifier_sequence
                | t.normal_identifier_sequence
                )
            )

            R( operator_identifier_sequence, std::string,
                make_keyword( "op" )[helper::construct<std::string>( "%op_" )]
                > -( make_keyword( "pre" )[helper::append( "pre_" )]
                   | make_keyword( "post" )[helper::append( "post_" )]
                   )
                > ( x3::lit( "==" )[helper::append( "==" )]
                  | x3::lit( "!=" )[helper::append( "!=" )]
                  | x3::lit( "||" )[helper::append( "||" )]
                  | x3::lit( "&&" )[helper::append( "&&" )]
                  | x3::lit( "<=" )[helper::append( "<=" )]
                  | x3::lit( ">=" )[helper::append( ">=" )]
                  | x3::lit( "<<" )[helper::append( "<<" )]
                  | x3::lit( ">>" )[helper::append( ">>" )]
                  | x3::lit( "()" )[helper::append( "()" )]
                  | x3::lit( "[]" )[helper::append( "[]" )]
                  | x3::lit( "|" )[helper::append( "|" )]
                  | x3::lit( "^" )[helper::append( "^" )]
                  | x3::lit( "&" )[helper::append( "&" )]
                  | x3::lit( "+" )[helper::append( "+" )]
                  | x3::lit( "-" )[helper::append( "-" )]
                  | x3::lit( "*" )[helper::append( "*" )]
                  | x3::lit( "/" )[helper::append( "/" )]
                  | x3::lit( "%" )[helper::append( "%" )]
                  | x3::lit( "<" )[helper::append( "<" )]
                  | x3::lit( ">" )[helper::append( ">" )]
                  | x3::lit( "=" )[helper::append( "=" )]
                  )
            )

            R( normal_identifier_sequence, std::string,
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
#undef RA
#undef RN

#endif /*RILL_SYNTAX_ANALYSIS_CODE_GRAMMAR_HPP*/
