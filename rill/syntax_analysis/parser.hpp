//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_PARSER_HPP
#define RILL_SYNTAX_ANALYSIS_PARSER_HPP

#include <string>
#include <vector>
#include <memory>
#include <iostream>

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3
#endif
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_as.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"

#include "../attribute/attribute.hpp"

//#include "environment.hpp"
#include "skip_parser.hpp"

#include "parser_helper.hpp"
#include "error.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace fusion = boost::fusion;
        namespace phx = boost::phoenix;
        namespace qi = boost::spirit::qi;
        namespace ascii = boost::spirit::ascii;

        //
        // grammer definition of Rill
        //
        template<typename Iterator>
        class code_grammer
            : public qi::grammar<Iterator, ast::statements_ptr(), skip_grammer<Iterator>>
        {
        public:
            using skip_grammer_type = skip_grammer<Iterator>;
            using rule_no_skip_no_type = qi::rule<Iterator>;
            template<typename T> using rule_no_skip = qi::rule<Iterator, T>;
            template<typename T> using rule = qi::rule<Iterator, T, skip_grammer_type>;

            struct t
            {
                t( Iterator const& head )
                    : position_annotator_( head )
                {}

            public:
                template<qi::error_handler_result E, typename Rule, typename T>
                auto operator()( Rule& rule, T const& n ) const
                    -> void
                {
                    rule.name( n );

                    auto const err_handler   = error_handler_( qi::_1, qi::_2, qi::_3, qi::_4 );
                    auto const pos_annotator = position_annotator_( qi::_val, qi::_1, qi::_3 );
                    qi::on_error<E>( rule, err_handler );
                    qi::on_success( rule, pos_annotator );
                }

                template<typename Rule, typename T>
                auto operator()( Rule& rule, T const& name ) const
                    -> void
                {
                    this->operator()<qi::accept>( rule, name );
                }

            private:
                phx::function<helper::make_position_annotator_lazy<Iterator>> position_annotator_;
                phx::function<error_handler_lazy<Iterator>> error_handler_;
            };

            t attr;

        public:
            code_grammer( Iterator const& head )
                : code_grammer::base_type( program_, "rill" )
                , attr( head )
            {
                using ascii::char_;
                using ascii::string;
                using namespace qi::labels;


                //
                program_
                    = ( top_level_statements_ > ( qi::eol | qi::eoi ) );
                attr( program_, "program" );

                //
                top_level_statement_
                    = ( function_definition_statement_
                      | class_definition_statement_
                      | extern_statement_
                      | empty_statement_
                      )
                    ;

                top_level_statements_.name( "top_level_statements" );
                top_level_statements_
                    = qi::as<ast::statement_list>()[
                        *top_level_statement_
                      ][qi::_val = helper::make_node_ptr<ast::statements>( qi::_1 )]
                    ;


                program_body_statement_
                    = ( block_statement_
                      | variable_declaration_statement_
                      | while_statement_
                      | if_statement_
                      | return_statement_
                      | jit_statement_
                      | empty_statement_
                      | expression_statement_     // NOTE: this statement must be set at last
                      )
                    ;

                program_body_statements_
                    = qi::as<ast::statement_list>()[
                        *program_body_statement_
                      ][qi::_val = helper::make_node_ptr<ast::statements>( qi::_1 )]
                    ;

                program_block_statement_
                    = program_body_statement_[
                        qi::_val = helper::make_node_ptr<ast::block_statement>( qi::_1 )
                       ]
                    ;


                class_body_statement_
                    = ( class_function_definition_statement_
                      | class_variable_declaration_statement_
                      | empty_statement_
                      )
                    ;

                class_body_statements_
                    = qi::as<ast::statement_list>()[
                        *class_body_statement_
                      ][qi::_val = helper::make_node_ptr<ast::statements>( qi::_1 )]
                    ;

                class_body_block_
                    = qi::lit( "{" ) >> class_body_statements_ >> qi::lit( "}" )
                    ;



                //
                function_body_block_
                    = (qi::lit( "{" ) >> program_body_statements_ >> qi::lit( "}" ) )[
                        qi::_val = qi::_1
                       ]
                    | (qi::lit( "=>" ) >> expression_statement_ )[
                        qi::_val = helper::make_node_ptr<ast::statements>( qi::_1 )
                       ]
                    ;



                //
                //
                //
                //
                block_statement_
                    = ( qi::lit( "{" )
                     >> program_body_statements_
                     >> qi::lit( "}" )
                      )[qi::_val = helper::make_node_ptr<ast::block_statement>( qi::_1 )]
                    ;


                empty_statement_.name( "empty_statement" );
                empty_statement_
                    = statement_termination_[qi::_val = helper::make_node_ptr<ast::empty_statement>()]
                    ;


                return_statement_.name( "return_statement" );
                return_statement_
                    = qi::lit( "return" )
                    > ( expression_ > statement_termination_ )[
                        qi::_val = helper::make_node_ptr<ast::return_statement>( qi::_1 )
                      ]
                    ;



                jit_statement_
                    = qi::lit( "jit" )
                    > ( expression_ > statement_termination_ )[
                        qi::_val = helper::make_node_ptr<ast::jit_statement>( qi::_1 )
                      ]
                    ;




                //function_body_expression_
                //
                function_definition_statement_.name( "function_definition_statement" );
                function_definition_statement_
                    = ( qi::lit( "def" )
                      > identifier_
                      > parameter_variable_declaration_list_
                      > -type_specifier_
                      > function_body_block_
                      )[
                          qi::_val = helper::make_node_ptr<ast::function_definition_statement>(
                              qi::_1,
                              qi::_2,
                              qi::_3,
                              qi::_4
                              )
                      ]
                    ;



                class_function_definition_statement_.name( "class_function_definition_statement" );
                class_function_definition_statement_
                    = ( qi::lit( "def" )
                      > identifier_
                      > parameter_variable_declaration_list_
                      > -type_specifier_
                      > function_body_block_
                      )[
                          qi::_val = helper::make_node_ptr<ast::class_function_definition_statement>(
                              qi::_1,
                              qi::_2,
                              qi::_3,
                              qi::_4
                              )
                      ]
                    ;








                class_definition_statement_.name( "class_definition_statement" );
                class_definition_statement_
                    = ( qi::lit( "class" )
                        >> identifier_
                        >> ( parameter_variable_declaration_list_ | qi::eps )    // constructor
                        >> class_body_block_
                      )[
                          qi::_val = helper::make_node_ptr<ast::class_definition_statement>(
                              qi::_1,
                              qi::_2,
                              qi::_3
                              )
                      ]
                    ;



                //
                extern_statement_.name( "extern_statement" );
                extern_statement_
                    = qi::lit( "extern" )
                    > ( extern_function_declaration_statement_
                      )
                    > statement_termination_
                    ;


                //
                extern_function_declaration_statement_.name( "extern_function_declaration_statement" );
                extern_function_declaration_statement_
                    = ( qi::lit( "def" )
                      > identifier_
                      > parameter_variable_declaration_list_
                      > type_specifier_
                      > string_literal_sequenece_
                      )[
                        qi::_val = helper::make_node_ptr<ast::extern_function_declaration_statement>(
                            qi::_1,
                            qi::_2,
                            qi::_3,
                            qi::_4
                            )
                      ]
                    ;

#if 0
                template_statement_
                    =
#endif

                while_statement_
                    = ( qi::lit( "while" )
                      > ( qi::lit( "(" ) > expression_ > qi::lit( ")" ) )
                      > program_block_statement_
                      )[
                          qi::_val = helper::make_node_ptr<ast::test_while_statement>(
                              qi::_1,
                              qi::_2
                              )
                      ]
                    ;



                if_statement_
                    = ( qi::lit( "if" )
                      > ( qi::lit( "(" ) > expression_ > qi::lit( ")" ) )
                      > program_block_statement_
                        > -(
                            qi::lit( "else" ) > program_block_statement_
                        )
                      )[
                          qi::_val = helper::make_node_ptr<ast::test_if_statement>(
                              qi::_1,
                              qi::_2,
                              qi::_3
                              )
                      ]
                    ;


                //
                variable_declaration_statement_
                    = qi::as<ast::variable_declaration>()[
                        variable_declaration_ > statement_termination_
                      ][
                          qi::_val = helper::make_node_ptr<ast::variable_declaration_statement>( qi::_1 )
                      ]
                    ;

                class_variable_declaration_statement_
                    = qi::as<ast::variable_declaration>()[
                        variable_declaration_ > statement_termination_
                      ][
                          qi::_val = helper::make_node_ptr<ast::class_variable_declaration_statement>( qi::_1 )
                      ]
                    ;



                //
                expression_statement_
                    = ( expression_ > statement_termination_ )[
                          qi::_val = helper::make_node_ptr<ast::expression_statement>( qi::_1 )
                      ]
                    ;


                type_attributes_
                    = quality_specifier_ ^ modifiability_specifier_ ^ qi::eps
                    ;

                //
                //
                //
                quality_specifier_
                    = qi::lit( "val" )[qi::_val = phx::val( attribute::quality_kind::k_val )]
                    | qi::lit( "ref" )[qi::_val = phx::val( attribute::quality_kind::k_ref )]
                    ;

/*
                variable_location_specifier_
                    = qi::lit( "temporary" )[qi::_val = phx::val( ast::variable_kind::val )]
                    | qi::lit( "stack" )[qi::_val = phx::val( ast::variable_kind::ref )]
                    | qi::lit( "gc" )[qi::_val = phx::val( ast::variable_kind::ref )]
                    | qi::lit( "unmanaged" )[qi::_val = phx::val( ast::variable_kind::ref )]
                    ;
*/


                modifiability_specifier_
                    = qi::lit( "mutable" )[qi::_val = phx::val( attribute::modifiability_kind::k_mutable )]
                    | qi::lit( "const" )[qi::_val = phx::val( attribute::modifiability_kind::k_const )]
                    | qi::lit( "immutable" )[qi::_val = phx::val( attribute::modifiability_kind::k_immutable )]
                    ;

                // ====
                //
                // ====
                variable_declaration_
                    %= quality_specifier_ > variable_initializer_unit_//list_
                    ;

/*                variable_initializer_unit_list_
                    = variable_initializer_unit_ % ','
                    ;
*/

                variable_initializer_unit_
                    %= identifier_ > value_initializer_unit_
                    ;

                // ====
                //
                // ====
                parameter_variable_declaration_
                    = quality_specifier_ > parameter_variable_initializer_unit_
                    ;

                parameter_variable_initializer_unit_
                    = -identifier_ > value_initializer_unit_
                    ;

                parameter_variable_declaration_list_.name( "parameter_variable_declaration_list" );
                parameter_variable_declaration_list_
                    = ( qi::lit( '(' ) >> qi::lit( ')' ) )
                    | ( qi::lit( '(' ) >> ( parameter_variable_declaration_ % ',' ) >> qi::lit( ')' ) )
                    ;


                // value initializer unit
                // Ex.
                /// = 5
                /// = 5 :int
                /// :int
                value_initializer_unit_.name( "value_initializer_unit" );
                value_initializer_unit_
                    = ( qi::lit( '=' ) > expression_ ) || type_specifier_
                    ;



                //
                type_specifier_.name( "type_specifier" );
                type_specifier_
                    = ( qi::lit( ':' ) > type_ )
                    ;


                // ==================================================
                // ==================================================
                //
                // ==================================================

                type_.name( "type" );
                type_
                    = type_identifier_expression_.alias()
                    ;

#if 0
                    = base_type_
                    | compiletime_type_expression_
                    ;

                type_expression_
                    = type_expression_priority_[TypeExpressionHierarchyNum-1]
                    ;

                {
                    // Postfix Expression
                    auto const priority = 1;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "[" ) >> expression_ >> qi:: )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "||", qi::_1 )]
                            )
                        ;

                    logical_or_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Primary Expression
                    auto const priority = 0;
                    expression_priority_[priority]
                        = qi::as<ast::value_ptr>()
                            [ identifier_
                            | identifier_with_root_
                            | template_instance_
                            | template_instance_with_root_
                            | numeric_literal_
                            | boolean_literal_
                            | string_literal_
                          ][ qi::_val = helper::make_node_ptr<ast::term_expression>( qi::_1 ) ]
                        | ( qi::lit( '(' ) >> expression_ >> qi::lit( ')' ) )[qi::_val = qi::_1]
                        ;

                    primary_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                base_type_
                    = ( compound_type_ >> type_attributes_ )[
                        qi::_val = helper::make_node_ptr<ast::type_identifier_expression>(
                            qi::_1,
                            qi::_2
                            )
                      ]
                    ;

                compound_type_
                    = base_types_
                    >> *( compound_type_postfix_ >> type_attributes_ )
                    ;

                base_types_
                    = nested_identifier_
                    | ( qi::lit( '(' ) >> compiletime_type_expression_ >> qi::lit( ')' ) )[qi::_val = qi::_1]
                    ;

                compound_type_postfix_
                    = ( qi::lit('[') >> -expression_ >> qi::lit(']')
                    ;

                compiletime_type_expression_
                    = ( assign_expression_ )[
                          qi::_val = helper::make_node_ptr<ast::compiletime_type_expression>( qi::_1 )
                      ]
                    ;
#endif


                //
                type_identifier_expression_
                    = ( nested_identifier_ >> type_attributes_ )[
                          qi::_val = helper::make_node_ptr<ast::type_identifier_expression>(
                              qi::_1,
                              qi::_2
                              )
                      ]
                    ;



                // ========================================


                expression_
                    %= expression_priority_[ExpressionHierarchyNum-1]
                    ;
                qi::debug( expression_ );


                {
                    // Comma Expression
                    auto const priority = 15;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "," ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, ",", qi::_1 )]
                            )
                        ;

                    commma_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Assign Expression
                    auto const priority = 14;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "=" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "=", qi::_1 )]
                            )
                        ;

                    assign_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Conditional Expression
                    auto const priority = 13;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        // TODO: add conditional operator( ? : )
                        ;

                    conditional_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Logical OR Expression
                    auto const priority = 12;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "||" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "||", qi::_1 )]
                            )
                        ;

                    logical_or_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Logical AND Expression
                    auto const priority = 11;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "&&" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "&&", qi::_1 )]
                            )
                        ;

                    logical_and_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Bitwise OR Expression
                    auto const priority = 10;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "|" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "|", qi::_1 )]
                            )
                        ;

                    bitwise_or_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Bitwise XOR Expression
                    auto const priority = 9;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "^" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "^", qi::_1 )]
                            )
                        ;

                    bitwise_xor_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Bitwise AND Expression
                    auto const priority = 8;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "&" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "&", qi::_1 )]
                            )
                        ;

                    bitwise_and_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Equality Expression
                    auto const priority = 7;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "==" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "==", qi::_1 )]
                            | ( qi::lit( "!=" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "!=", qi::_1 )]
                            )
                        ;

                    equality_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Relational Expression
                    auto const priority = 6;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "<" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "<", qi::_1 )]
                            | ( qi::lit( "<=" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "<=", qi::_1 )]
                            | ( qi::lit( ">" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, ">", qi::_1 )]
                            | ( qi::lit( ">=" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, ">=", qi::_1 )]
                            )
                        ;

                    relational_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Shift Expression
                    auto const priority = 5;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "<<" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "<<", qi::_1 )]
                            | ( qi::lit( ">>" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, ">>", qi::_1 )]
                            )
                        ;

                    shift_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Add/Sub Expression
                    auto const priority = 4;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "+" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "+", qi::_1 )]
                            | ( qi::lit( "-" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "-", qi::_1 )]
                            )
                        ;

                    add_sub_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Mul/Div/Rem Expression
                    auto const priority = 3;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "*" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "*", qi::_1 )]
                            | ( qi::lit( "/" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "/", qi::_1 )]
                            | ( qi::lit( "%" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "%", qi::_1 )]
                            )
                        ;

                    mul_div_rem_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Unary Expression
                    auto const priority = 2;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        ;

                    unary_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }

                {
                    // Postfix Expression
                    auto const priority = 1;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "." )
                             >> qi::as<ast::identifier_value_base_ptr>()
                                [ identifier_
                                | identifier_with_root_
                                | template_instance_
                                | template_instance_with_root_
                                ]
                              )[
                                  qi::_val = helper::make_node_ptr<ast::element_selector_expression>(
                                      qi::_val,
                                      qi::_1
                                      )
                               ]

                              | ( qi::lit( "[" ) > -expression_ > qi::lit( "]" ) )[
                                  qi::_val = helper::make_node_ptr<ast::subscrpting_expression>(
                                      qi::_val,
                                      qi::_1
                                      )
                               ]

                            | ( argument_list_ )[
                                  qi::_val = helper::make_node_ptr<ast::call_expression>(
                                      qi::_val,
                                      qi::_1
                                      )
                              ]
                            )
                        ;


                    postfix_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }


                {
                    // Primary Expression
                    auto const priority = 0;
                    expression_priority_[priority]
                        = qi::as<ast::value_ptr>()
                            [ identifier_
                            | identifier_with_root_
                            | template_instance_
                            | template_instance_with_root_
                            | numeric_literal_
                            | boolean_literal_
                            | string_literal_
                          ][ qi::_val = helper::make_node_ptr<ast::term_expression>( qi::_1 ) ]
                        | ( qi::lit( '(' ) >> expression_ >> qi::lit( ')' ) )[qi::_val = qi::_1]
                        ;

                    primary_expression_ = expression_priority_[priority].alias();

                    qi::debug( expression_priority_[priority] );
                }



                nested_identifier_
                    = qi::as<std::vector<ast::identifier_value_base_ptr>>()[
                          ( identifier_ | template_instance_ ) % qi::lit( '.' )
                      ][
                          qi::_val = helper::make_node_ptr<ast::nested_identifier_value>( qi::_1 )
                      ]
                    ;

                // TODO: add "nested_identifier_with_root_"

                //
                integer_literal_
                    = ( qi::int_ )[
                          qi::_val = helper::make_literal_value_ptr<ast::intrinsic::int32_value>( qi::_1 )
                      ];

                numeric_literal_
                    = integer_literal_
                    ;

                boolean_literal_
                    = qi::lit( "true" )[qi::_val = helper::make_literal_value_ptr<ast::intrinsic::boolean_value>( phx::val( true ) )]
                    | qi::lit( "false" )[qi::_val = helper::make_literal_value_ptr<ast::intrinsic::boolean_value>( phx::val( false ) )];
                    ;
/*
                type_
                  =
*/

                //
                string_literal_
                    = string_literal_sequenece_[
                          qi::_val = helper::make_literal_value_ptr<ast::intrinsic::string_value>( qi::_1 )
                      ]
                    ;


                string_literal_sequenece_
                    = qi::as_string[qi::lexeme[ qi::lit('"') >> *( ( escape_sequence_ | qi::char_ )- '"') >> qi::lit('"') ]];


                // TODO: support escape sequence
                escape_sequence_
                    = qi::lit( "\\n" )[qi::_val = phx::val( '\n' )]
                    ;



        /**/
                argument_list_.name( "argument_list" );
                argument_list_
                    = ( qi::lit( '(' ) >> qi::lit( ')' ) )
                    | ( qi::lit( '(' ) >> ( assign_expression_ % ',' ) >> qi::lit( ')' ) )
                    ;



                //
                //
                //
                identifier_.name( "identifier" );
                identifier_
                    = native_symbol_string_/*TODO: fix...*/[
                          qi::_val = helper::make_node_ptr<ast::identifier_value>( qi::_1 )
                      ]
                    ;
                identifier_with_root_
                    = qi::lit( '.' )
                   >> native_symbol_string_/*TODO: fix...*/[
                          qi::_val = helper::make_node_ptr<ast::identifier_value>( qi::_1, phx::val( true ) )
                      ]
                    ;


                template_instance_
                    = native_symbol_string_/*TODO: fix...*/[
                          qi::_val = helper::make_node_ptr<ast::template_instance_value>( qi::_1 )
                      ]
                    ;
                template_instance_with_root_
                    = qi::lit( '.' )
                   >> native_symbol_string_/*TODO: fix...*/[
                          qi::_val = helper::make_node_ptr<ast::template_instance_value>( qi::_1, phx::val( true ) )
                      ]
                    ;

                // instanced_identifier

                // static_identifier_



                // template_identifier_
                native_symbol_.name( "native_symbol" );
                native_symbol_
                    = native_symbol_string_[
                          qi::_val = helper::make_node_ptr<ast::intrinsic::symbol_value>( qi::_1 )
                      ]
                    ;

                native_symbol_string_.name( "native_symbol_string" );
                native_symbol_string_
                    = qi::lexeme[ ascii::char_( "a-zA-Z_" ) >> *ascii::char_( "a-zA-Z0-9_" ) ]
                    ;

                //
                statement_termination_.name( "semicolon" );
                statement_termination_ = qi::lit( ';' );

            }

        private:
            rule<ast::statements_ptr()> program_;
            rule<ast::statement_ptr()> top_level_statement_;
            rule<ast::statements_ptr()> top_level_statements_;

            rule<ast::statement_ptr()> program_body_statement_;
            rule<ast::statements_ptr()> program_body_statements_;
            rule<ast::block_statement_ptr()> program_block_statement_;

            rule<ast::statement_ptr()> class_body_statement_;
            rule<ast::statements_ptr()> class_body_statements_;

            rule<ast::block_statement_ptr()> block_statement_;

            rule<ast::function_definition_statement_ptr()> function_definition_statement_;
            rule<ast::statements_ptr()> function_body_block_;

            rule<ast::variable_declaration_statement_ptr()> variable_declaration_statement_;

            rule<ast::extern_statement_base_ptr()> extern_statement_;
            rule<ast::extern_function_declaration_statement_ptr()> extern_function_declaration_statement_;

            rule<ast::class_definition_statement_ptr()> class_definition_statement_;
            rule<ast::statements_ptr()> class_body_block_;

            rule<ast::class_function_definition_statement_ptr()> class_function_definition_statement_;
            rule<ast::class_variable_declaration_statement_ptr()> class_variable_declaration_statement_;


            rule<ast::return_statement_ptr()> return_statement_;
            rule<ast::jit_statement_ptr()> jit_statement_; // experimental
            rule<ast::expression_statement_ptr()> expression_statement_;
            rule<ast::empty_statement_ptr()> empty_statement_;

            // test
            rule<ast::test_while_statement_ptr()> while_statement_;
            rule<ast::test_if_statement_ptr()> if_statement_;


            rule<attribute::type_attributes_optional()> type_attributes_;

            rule<attribute::quality_kind()> quality_specifier_;
            rule<attribute::modifiability_kind()> modifiability_specifier_;


            rule<ast::variable_declaration()> variable_declaration_;
            rule<ast::variable_declaration_unit()> variable_initializer_unit_;
            rule<ast::variable_declaration_unit_list()> variable_initializer_unit_list_;

            rule<ast::variable_declaration()> parameter_variable_declaration_;
            rule<ast::variable_declaration_unit()> parameter_variable_initializer_unit_;

            rule<ast::parameter_list()> parameter_variable_declaration_list_;

            rule<ast::value_initializer_unit()> value_initializer_unit_;
            rule<ast::type_expression_ptr()> type_specifier_;

            rule<ast::expression_ptr()> expression_;
            static std::size_t const ExpressionHierarchyNum = 16;
            rule<ast::expression_ptr()> expression_priority_[ExpressionHierarchyNum];
            rule<ast::expression_ptr()> commma_expression_;
            rule<ast::expression_ptr()> assign_expression_;
            rule<ast::expression_ptr()> conditional_expression_;
            rule<ast::expression_ptr()> logical_or_expression_;
            rule<ast::expression_ptr()> logical_and_expression_;
            rule<ast::expression_ptr()> bitwise_or_expression_;
            rule<ast::expression_ptr()> bitwise_xor_expression_;
            rule<ast::expression_ptr()> bitwise_and_expression_;
            rule<ast::expression_ptr()> equality_expression_;
            rule<ast::expression_ptr()> relational_expression_;
            rule<ast::expression_ptr()> shift_expression_;
            rule<ast::expression_ptr()> add_sub_expression_;
            rule<ast::expression_ptr()> mul_div_rem_expression_;
            rule<ast::expression_ptr()> unary_expression_;
            rule<ast::expression_ptr()> postfix_expression_;
            rule<ast::expression_ptr()> primary_expression_;


            rule<ast::expression_list()> argument_list_;


#if 0
            rule<ast::type_expression_ptr()> expression_;
            static std::size_t const ExpressionHierarchyNum = 16;
            rule<ast::expression_ptr()> expression_priority_[ExpressionHierarchyNum];
            rule<ast::expression_ptr()> commma_expression_;
            rule<ast::expression_ptr()> assign_expression_;
            rule<ast::expression_ptr()> conditional_expression_;
            rule<ast::expression_ptr()> logical_or_expression_;
            rule<ast::expression_ptr()> logical_and_expression_;
            rule<ast::expression_ptr()> bitwise_or_expression_;
            rule<ast::expression_ptr()> bitwise_xor_expression_;
            rule<ast::expression_ptr()> bitwise_and_expression_;
            rule<ast::expression_ptr()> equality_expression_;
            rule<ast::expression_ptr()> relational_expression_;
            rule<ast::expression_ptr()> shift_expression_;
            rule<ast::expression_ptr()> add_sub_expression_;
            rule<ast::expression_ptr()> mul_div_rem_expression_;
            rule<ast::expression_ptr()> unary_expression_;
            rule<ast::expression_ptr()> postfix_expression_;
            rule<ast::expression_ptr()> primary_expression_;
#endif

            rule<ast::type_expression_ptr()> type_;
            rule<ast::type_identifier_expression_ptr()> type_identifier_expression_;
            rule<ast::compiletime_return_type_expression_ptr()> compiletime_return_type_expression_;


            // rule<ast::variable_value_ptr()> variable_value_;

            rule<ast::literal_value_ptr()> numeric_literal_;
            rule<ast::literal_value_ptr()> integer_literal_;
            rule<ast::literal_value_ptr()> boolean_literal_;
            rule<ast::literal_value_ptr()> string_literal_;

            rule<ast::nested_identifier_value_ptr()> nested_identifier_;
            rule<ast::identifier_value_ptr()> identifier_, identifier_with_root_;
            rule<ast::template_instance_value_ptr()> template_instance_, template_instance_with_root_;

            rule_no_skip<ast::intrinsic::symbol_value_ptr()> native_symbol_;
            rule_no_skip<ast::native_string_t()> native_symbol_string_;

            rule_no_skip<ast::native_string_t()> string_literal_sequenece_;


            rule_no_skip<char()> escape_sequence_;

            rule_no_skip_no_type statement_termination_;
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_PARSER_HPP*/
