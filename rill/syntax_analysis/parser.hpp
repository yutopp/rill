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

#define BOOST_SPIRIT_USE_PHOENIX_V3 1
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_as.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>

#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"
#include "../ast/root.hpp"

//#include "environment.hpp"
#include "skip_parser.hpp"

#include "parser_helper.hpp"


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
        template<typename StringT, typename Iterator>
        class code_grammer
            : public qi::grammar<Iterator, ast::statement_list(), skip_grammer<Iterator>>
        {
        public:
            using skip_grammer_type = skip_grammer<Iterator>;
            using rule_no_skip_no_type = qi::rule<Iterator>;
            template<typename T> using rule_no_skip = qi::rule<Iterator, T>;
            template<typename T> using rule = qi::rule<Iterator, T, skip_grammer_type>;

        public:
            code_grammer()
                : code_grammer::base_type( program_, "rill" )
            {
                using ascii::char_;
                using ascii::string;
                using namespace qi::labels;

                //
                //program_.name( "program" );
//                program_ %= top_level_statements_ > ( qi::eol | qi::eoi );// 
                program_ = ( top_level_statements_ > ( qi::eol | qi::eoi ) );//[qi::_val = qi::_1];// 

                //
                top_level_statements_.name( "top_level_statements" );
                top_level_statements_
                    %= *( function_definition_statement_
                        | extern_statement_
                        | empty_statement_
                        | expression_statement_     // NOTE: this statement must be set at last
                        )
                    ;
        
                function_body_statements_
                    %= *( variable_declaration_statement_
                        | return_statement_
                        | empty_statement_
                        | expression_statement_     // NOTE: this statement must be set at last
                        )
                    ;






                //
                //
                //
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
                    = (
                        ( qi::lit( "def" )
                        > identifier_
                        > parameter_variable_declaration_list_
                        > -( qi::lit( ":" ) >> identifier_ )
                        > string_literal_sequenece_
                        )
                      )[
                        qi::_val
                            = helper::make_node_ptr<ast::extern_function_declaration_statement>(
                                qi::_1,
                                qi::_2,
                                qi::_3,
                                qi::_4
                                )
                      ]
                    ;

                //
                function_body_block_
                    %= qi::lit( "{" ) >> function_body_statements_ >> qi::lit( "}" )
                    ;

                //function_body_expression_
                //
                function_definition_statement_.name( "function_definition_statement" );
                function_definition_statement_
                    = ( qi::lit( "def" )
                      > identifier_
                      > parameter_variable_declaration_list_
                      > -( qi::lit( ":" ) >> identifier_ )
                      > ( function_body_block_/* | expression_*/ )
                      )[
                        qi::_val
                            = helper::make_node_ptr<ast::function_definition_statement>(
                                qi::_1,
                                qi::_2,
                                qi::_3,
                                qi::_4
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

                //
                expression_statement_
                    = ( expression_ > statement_termination_ )[
                        qi::_val = helper::make_node_ptr<ast::expression_statement>( qi::_1 )
                      ]
                    ;


                //
                //
                //
                variable_kind_specifier_
                    = qi::lit( "val" )[qi::_val = phx::val( ast::variable_kind::k_val )]
                    | qi::lit( "ref" )[qi::_val = phx::val( ast::variable_kind::k_ref )]
                    ;

/*
                variable_location_specifier_
                    = qi::lit( "temporary" )[qi::_val = phx::val( ast::variable_kind::val )]
                    | qi::lit( "stack" )[qi::_val = phx::val( ast::variable_kind::ref )]
                    | qi::lit( "gc" )[qi::_val = phx::val( ast::variable_kind::ref )]
                    | qi::lit( "unmanaged" )[qi::_val = phx::val( ast::variable_kind::ref )]
                    ;
*/
/*
                type_attributes_
                    = modifiability_specifier_ ^ qi::eps
                    ;

                modifiability_specifier_
                    = qi::lit( "mutable" )[qi::_val = phx::val( ast::modifiability_attribute_kind::k_mutable )]
                    | qi::lit( "const" )[qi::_val = phx::val( ast::modifiability_attribute_kind::k_const )]
                    | qi::lit( "immutable" )[qi::_val = phx::val( ast::modifiability_attribute_kind::k_immutable )]
                    ;
*/

                // ====
                //
                // ====
                variable_declaration_
                    = ( variable_kind_specifier_ > variable_initializer_unit_ )//list_
                    ;

/*                variable_initializer_unit_list_
                    = variable_initializer_unit_ % ','
                    ;
*/

                variable_initializer_unit_
                    = ( single_identifier_ > value_initializer_unit_ )
                    ;

                // ====
                //
                // ====
                parameter_variable_declaration_
                    = variable_kind_specifier_ > parameter_variable_initializer_unit_
                    // TODO: specifier should be OPTIONAL(default =ref)
                    ;

                parameter_variable_initializer_unit_
                    = -single_identifier_ > value_initializer_unit_
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
                    = ( qi::lit( ':' ) > type_expression_ )
                    ;




                //
                type_expression_.name( "type_expression" );
                type_expression_
                    = type_identifier_expression_ | compiletime_return_type_expression_
                    ;

                //
                type_identifier_expression_
                    = identifier_[
                        qi::_val = helper::make_node_ptr<ast::type_identifier_expression>( qi::_1 )
                      ]
                    ;

                //
                compiletime_return_type_expression_
                    = ( qi::lit( '^' ) > expression_ )[
                        qi::_val = helper::make_node_ptr<ast::compiletime_return_type_expression>( qi::_1 )
                      ]
                    ;



                expression_
                    = expression_priority_[ExpressionHierarchyNum-1]
                    ;

                {
                    auto const priority = 3u;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "+" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "+", qi::_1 )]
                            | ( qi::lit( "-" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "-", qi::_1 )]
                            )
                        ;
                }

                {
                    auto const priority = 2u;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "*" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "*", qi::_1 )]
                            | ( qi::lit( "/" ) >> expression_priority_[priority-1] )[qi::_val = helper::make_binary_op_node_ptr( qi::_val, "/", qi::_1 )]
                            )
                        ;
                }

                {
                    auto const priority = 1u;
                    expression_priority_[priority]
                       = call_expression_
                        | expression_priority_[priority-1]
        //                | 
                        ;
                }


                {
                    auto const priority = 0u;
                    expression_priority_[priority]
                        %= term_expression_[qi::_val = qi::_1]
                        | ( ( qi::lit( '(' ) >> expression_ >> qi::lit( ')' ) ) )[qi::_val = qi::_1]
                        ;
                }


                // call expression
                call_expression_
                    = ( identifier_
                      >> argument_list_
                      )[
                          qi::_val = helper::make_node_ptr<ast::call_expression>( qi::_1, qi::_2 )
                      ]
                    ;

                // termination
                term_expression_
                    = ( integer_literal_[ qi::_val = helper::make_node_ptr<ast::term_expression>( qi::_1 ) ]
                      | string_literal_[ qi::_val = helper::make_node_ptr<ast::term_expression>( qi::_1 ) ]
                      | variable_value_[ qi::_val = helper::make_node_ptr<ast::term_expression>( qi::_1 ) ]
                      )
                    ;

                //
                variable_value_
                    = identifier_[
                        qi::_val = helper::make_node_ptr<ast::variable_value>( qi::_1 )
                      ]
                    ;

                //
                integer_literal_
                    = ( qi::int_ )[
                        qi::_val = helper::make_intrinsic_value_ptr<ast::intrinsic::int32_value>( qi::_1 )
                      ];

                //
                string_literal_
                    = string_literal_sequenece_[
                        qi::_val = helper::make_intrinsic_value_ptr<ast::intrinsic::string_value>( qi::_1 )
                      ]
                    ;

                // TODO: support escape sequence
                string_literal_sequenece_
                    = qi::as_string[qi::lexeme[ qi::lit('"') >> *( qi::char_ - '"') >> qi::lit('"') ]];

                //auto p = ( -native_symbol_ )[ phx::if_else( qi::_0, phx::construct<intrinsic::symbol_value_ptr>(), phx::construct<intrinsic::symbol_value_ptr>() )]

                /*
                auto
                value_constructor_
                    = ( qi::string( "local" ) | qi::string( "heap" ) | qi::string( "gc" ) | qi::eps[phx::val( "local" )] )
                    > identifier_
                    > -argument_list_
                    ;

                auto
                variable_definition_
                    = ( qi::string( "let" ) | qi::string( "const" ) | qi::string( "mutable" ) )
                    > -qi::lit( "ref" )
                    > single_identifier_
                    > -( ( qi::lit( '=' ) >> expression_ )
                       | ( qi::lit( ':' ) >> value_constructor_ )
                       )
                    ;

                auto
                parameter_variable_definition_
                    = ( qi::string( "const" ) | qi::string( "mutable" ) | qi::eps[phx::val( "let" )] )
                    > -qi::lit( "ref" )
                    > single_identifier_
                    > ( qi::lit( ':' ) >> value_constructor_ )
                    ;
*/


        /**/
                argument_list_.name( "argument_list" );
                argument_list_
                    = ( qi::lit( '(' ) >> qi::lit( ')' ) )
                    | ( qi::lit( '(' ) >> ( expression_ % ',' ) >> qi::lit( ')' ) )
                    ;



                //
                //
                //
                identifier_.name( "identifier" );
                identifier_
                    = single_identifier_[qi::_val = phx::bind( &ast::intrinsic::make_identifier, qi::_1)]
                    ;
                    // TODO: should +( single_identifier_ | single_template_identifier_ )

                // instanced_identifier

                // static_identifier_

                single_identifier_
                    = native_symbol_string_[
                        qi::_val = helper::make_node_ptr<ast::intrinsic::single_identifier_value>( qi::_1 )
                      ]
                    ;

                // template_identifier_
                native_symbol_.name( "native_symbol" );
                native_symbol_
                    = native_symbol_string_[
                        qi::_val = helper::make_node_ptr<ast::intrinsic::symbol_value>( qi::_1 )
                      ]
                    ;

                native_symbol_string_.name( "native_symbol_string" );
                native_symbol_string_
                    = qi::lexeme[ ascii::char_( "a-zA-Z_" ) >> *(ascii::alnum | ascii::char_( "_" )) ] // TODO: add '_' charactor
                    ;

                //
                statement_termination_.name( "semicolon" );
                statement_termination_ = qi::lit( ';' );


        /*      
                // identifiers
                holder_.name( "holder" );
                holder_ %= qi::lexeme[ qi::lit('!') >> ( lower_symbol_ | upper_symbol_ ) ];

                pointer_.name( "pointer" );
                pointer_ %= qi::lexeme[ qi::lit('!') >> qi::lit('*') >> ( lower_symbol_ | upper_symbol_ ) ];


                // symbols
                lower_symbol_.name( "lower_symbol" );
                lower_symbol_ %= qi::lexeme[ ascii::char_("a-z") >> *ascii::alnum ];

                upper_symbol_.name( "upper_symbol" );
                upper_symbol_ %= qi::lexeme[ ascii::char_("A-Z") >> *ascii::alnum ];*/


                // error handring...
                // if failed, show error messages and force accept this grammer step.
                qi::on_error<qi::accept>
                (
                    program_,
                    std::cout
                        << phx::val( "Error! Expecting " )
                        << _4
                        << phx::val( "\n" )                          // what failed?
                        << phx::val( "here: \'" )
                        << phx::construct<std::string>( _3, _2 )    // iterators to error-pos, end
                        << phx::val( "\'" )
                        << std::endl
                );
            }

        private:
            rule<ast::statement_list()> program_;

            rule<ast::statement_list()> top_level_statements_, function_body_statements_;
            rule<ast::statement_list()> function_body_block_, function_body_expression_;


            rule<ast::function_definition_statement_ptr()> function_definition_statement_;
            rule<ast::variable_declaration_statement_ptr()> variable_declaration_statement_;
            rule<ast::extern_statement_base_ptr()> extern_statement_;
            rule<ast::extern_function_declaration_statement_ptr()> extern_function_declaration_statement_;
            rule<ast::return_statement_ptr()> return_statement_;
            rule<ast::expression_statement_ptr()> expression_statement_;
            rule<ast::empty_statement_ptr()> empty_statement_;


            rule<ast::variable_kind()> variable_kind_specifier_;

            rule<ast::type_attributes()> type_attributes_;
            rule<ast::modifiability_attribute_kind()> modifiability_specifier_;

            rule<ast::variable_declaration()> variable_declaration_;
            rule<ast::variable_declaration_unit()> variable_initializer_unit_;
            rule<ast::variable_declaration_unit_list()> variable_initializer_unit_list_;

            rule<ast::variable_declaration()> parameter_variable_declaration_;
            rule<ast::variable_declaration_unit()> parameter_variable_initializer_unit_;

            rule<ast::parameter_list()> parameter_variable_declaration_list_;

            rule<ast::value_initializer_unit()> value_initializer_unit_;
            rule<ast::type_expression_ptr()> type_specifier_;

            static std::size_t const ExpressionHierarchyNum = 4;
            rule<ast::expression_ptr()> expression_, expression_priority_[ExpressionHierarchyNum];
            rule<ast::expression_list()> argument_list_;
            rule<ast::call_expression_ptr()> call_expression_;
            rule<ast::term_expression_ptr()> term_expression_;


            rule<ast::type_expression_ptr()> type_expression_;
            rule<ast::type_identifier_expression_ptr()> type_identifier_expression_;
            rule<ast::compiletime_return_type_expression_ptr()> compiletime_return_type_expression_;


            rule<ast::variable_value_ptr()> variable_value_;

            rule<ast::intrinsic_value_ptr()> integer_literal_;
            rule<ast::intrinsic_value_ptr()> string_literal_;

            rule<ast::intrinsic::identifier_value_ptr()> identifier_;
            rule<ast::intrinsic::single_identifier_value_ptr()> single_identifier_;

            rule_no_skip<ast::intrinsic::symbol_value_ptr()> native_symbol_;
            rule_no_skip<ast::native_string_t()> native_symbol_string_;

            rule_no_skip<ast::native_string_t()> string_literal_sequenece_;

            rule_no_skip_no_type statement_termination_;
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_PARSER_HPP*/
