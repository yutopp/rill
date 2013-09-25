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

//#include <boost/fusion/include/adapt_struct.hpp>

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"
#include "../ast/root.hpp"

//#include "environment.hpp"
#include "skip_parser.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace fusion = boost::fusion;
        namespace phx = boost::phoenix;
        namespace qi = boost::spirit::qi;
        namespace ascii = boost::spirit::ascii;


// TODO: move to elsewhere
auto make_binary_operator_tree( ast::expression_ptr const& lhs, ast::native_string_t const& op, ast::expression_ptr const& rhs )
    -> ast::expression_ptr
{
    return std::make_shared<ast::binary_operator_expression>(
            lhs,
            ast::intrinsic::make_binary_operator_identifier( op ),
            rhs
            );
}


        template<typename StringT, typename Iterator>
        class code_grammer
            : public qi::grammar<Iterator, ast::statement_list(), skip_grammer<Iterator>>
        {
        public:
            typedef skip_grammer<Iterator>     skip_grammer_type;

        public:
            code_grammer()
                : code_grammer::base_type( program_, "rill" )
            {
                using ascii::char_;
                using ascii::string;
                using namespace qi::labels;

                //
                program_.name( "program" );
                program_ %= top_level_statements_ > ( qi::eol | qi::eoi );// 

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
                    %= *( return_statement_
                        | empty_statement_
                        | expression_statement_     // NOTE: this statement must be set at last
                        )
                    ;
                //
                //block_ = 

                //top_level_statement_
                /*
                variable_definition_statement_
                    =

                value_type_variable_declaration_introducer_
                    = qi::lit( "val" )
                    //> ( /* add quarifiers... * )
                    > 
                    ;

                //
                reference_type_variable_declaration_introducer_
                    = qi::lit( "ref" )
                    //> ( /* add quarifiers... * )
                    ;
*/

                variable_declaration_
                    = variable_initializer_unit_
                    ;

                parameter_variable_declaration_
                    = variable_parameter_initializer_unit_
                    ;

                parameter_variable_declaration_list_.name( "parameter_variable_declaration__list" );
                parameter_variable_declaration_list_
                    = ( qi::lit( '(' ) >> qi::lit( ')' ) )
                    | ( qi::lit( '(' ) >> ( parameter_variable_declaration_ % ',' ) >> qi::lit( ')' ) )
                    ;

                //
                variable_initializer_unit_
                    = single_identifier_ > value_initializer_unit_
                    ;

                variable_parameter_initializer_unit_
                    = -single_identifier_ > value_initializer_unit_
                    ;



                value_initializer_unit_.name( "value_initializer_unit" );
                value_initializer_unit_
                    = ( qi::lit( '=' ) > expression_ ) || type_specifier_
                    ;



                //
                type_specifier_.name( "type_specifier" );
                type_specifier_
                    = ( qi::lit( ':' ) > type_expression_ )
                    ;

                empty_statement_.name( "empty_statement" );
                empty_statement_
                    = statement_termination_[
                        qi::_val
                            = phx::construct<ast::empty_statement_ptr>(
                                phx::new_<ast::empty_statement>()
                                )
                      ]
                    ;

                return_statement_.name( "return_statement" );
                return_statement_
                    = qi::lit( "return" )
                    > ( expression_ > statement_termination_ )[
                        qi::_val
                            = phx::construct<ast::return_statement_ptr>(
                                phx::new_<ast::return_statement>(
                                    qi::_1
                                    )
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
                    = (
                        ( qi::lit( "def" )
                        > identifier_
                        > parameter_variable_declaration_list_
                        > -( qi::lit( ":" ) >> identifier_ )
                        > string_literal_sequenece_
                        )
                      )[
                        qi::_val
                            = phx::construct<ast::extern_function_declaration_statement_ptr>(
                                phx::new_<ast::extern_function_declaration_statement>(
                                    qi::_1,
                                    qi::_2,
                                    qi::_3,
                                    qi::_4
                                    )
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
                            = phx::construct<ast::function_definition_statement_ptr>(
                                phx::new_<ast::function_definition_statement>(
                                    qi::_1,
                                    qi::_2,
                                    qi::_3,
                                    qi::_4
                                    )
                                )
                      ]
                    ;


                //
                expression_statement_
                    = ( expression_ > statement_termination_ )[
                        qi::_val
                            = phx::construct<ast::expression_statement_ptr>(
                                phx::new_<ast::expression_statement>(
                                    qi::_1
                                    )
                                )
                      ]
                    ;




                //
                type_expression_.name( "type_expression" );
                type_expression_
                    = type_identifier_expression_ | compiletime_return_type_expression_
                    ;

                //
                type_identifier_expression_
                    = identifier_[
                        qi::_val
                            = phx::construct<ast::type_identifier_expression_ptr>(
                                phx::new_<ast::type_identifier_expression>(
                                    qi::_1
                                    )
                                )
                      ]
                    ;

                //
                compiletime_return_type_expression_
                    = ( qi::lit( '^' ) > expression_ )[
                        qi::_val
                            = phx::construct<ast::compiletime_return_type_expression_ptr>(
                                phx::new_<ast::compiletime_return_type_expression>(
                                    qi::_1
                                    )
                                )
                      ]
                    ;



                expression_
                    %= expression_priority_[ExpressionHierarchyNum-1]
                    ;

                {
                    auto const priority = 3u;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "+" ) >> expression_priority_[priority-1] )[qi::_val = phx::bind( &make_binary_operator_tree, qi::_val, "+", qi::_1 )]
                            | ( qi::lit( "-" ) >> expression_priority_[priority-1] )[qi::_val = phx::bind( &make_binary_operator_tree, qi::_val, "-", qi::_1 )]
                            )
                        ;
                }

                {
                    auto const priority = 2u;
                    expression_priority_[priority]
                        = expression_priority_[priority-1][qi::_val = qi::_1]
                        >> *( ( qi::lit( "*" ) >> expression_priority_[priority-1] )[qi::_val = phx::bind( &make_binary_operator_tree, qi::_val, "*", qi::_1 )]
                            | ( qi::lit( "/" ) >> expression_priority_[priority-1] )[qi::_val = phx::bind( &make_binary_operator_tree, qi::_val, "/", qi::_1 )]
                            )
                        ;
                }

                {
                    auto const priority = 1u;
                    expression_priority_[priority]
                       %= call_expression_
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
                        qi::_val
                            = phx::construct<ast::call_expression_ptr>(
                                phx::new_<ast::call_expression>(
                                    qi::_1,
                                    qi::_2
                                    )
                                )
                      ]
                    ;

                // termination
                term_expression_
                    = ( integer_literal_[ qi::_val = phx::construct<ast::term_expression_ptr>( phx::new_<ast::term_expression>( qi::_1 ) ) ]
                      | string_literal_[ qi::_val = phx::construct<ast::term_expression_ptr>( phx::new_<ast::term_expression>( qi::_1 ) ) ]
                      | variable_value_[ qi::_val = phx::construct<ast::term_expression_ptr>( phx::new_<ast::term_expression>( qi::_1 ) ) ]
                      )
                    ;

                //
                variable_value_
                    = identifier_[
                        qi::_val
                            = phx::construct<ast::variable_value_ptr>(
                                phx::new_<ast::variable_value>(
                                    qi::_1
                                    )
                                )
                      ]
                    ;

                //
                integer_literal_
                    = ( qi::int_ )[
                        qi::_val
                            = phx::construct<ast::intrinsic_value_ptr>(
                                phx::new_<ast::intrinsic_value>(
                                    phx::construct<ast::intrinsic::int32_value_ptr>(
                                        phx::new_<ast::intrinsic::int32_value>(
                                            qi::_1
                                            )
                                        )
                                    )
                                )
                      ];

                //
                string_literal_
                    = string_literal_sequenece_[
                        qi::_val
                            = phx::construct<ast::intrinsic_value_ptr>(
                                phx::new_<ast::intrinsic_value>(
                                    phx::construct<ast::intrinsic::string_value_ptr>(
                                        phx::new_<ast::intrinsic::string_value>(
                                            qi::_1
                                            )
                                        )
                                    )
                                )
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
                        qi::_val
                            = phx::construct<ast::intrinsic::single_identifier_value_ptr>(
                                phx::new_<ast::intrinsic::single_identifier_value>(
                                    qi::_1
                                    )
                                )
                      ]
                    ;

                // template_identifier_

                native_symbol_.name( "native_symbol" );
                native_symbol_
                    = native_symbol_string_[
                        qi::_val
                            = phx::construct<ast::intrinsic::symbol_value_ptr>(
                                phx::new_<ast::intrinsic::symbol_value>(
                                    qi::_1
                                    )
                                )
                      ]
                    ;

                native_symbol_string_.name( "native_symbol_string" );
                native_symbol_string_
                    = qi::lexeme[ ascii::char_( "a-zA-Z_" ) >> *(ascii::alnum | ascii::char_( "_" )) ] // TODO: add '_' charactor
                    ;

                //
                statement_termination_.name( "semicolon" );
                statement_termination_ = qi::lit( ';' );


        /*        //
                top_level_statement_.name( "top_level_statement" );
                top_level_statement_ %= (
                    assignment_statement_
                    ) > ( qi::eol | qi::eoi );
        

                //
                assignment_statement_.name( "" );
                assignment_statement_ %= ( qi::hold[holder_] | pointer_ ) > qi::lit('=') > ( expression_ );


                //
                expression_.name( "expression" );
                expression_ %= holder_;


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
                        << _4 << std::endl                          // what failed?
                        << phx::val( "here: '" )
                        << phx::construct<std::string>( _3, _2 )    // iterators to error-pos, end
                        << phx::val( "'" )
                        << std::endl
                );
            }

        private:
            qi::rule<Iterator, ast::statement_list(), skip_grammer_type> program_;

            qi::rule<Iterator, ast::statement_list(), skip_grammer_type> top_level_statements_, function_body_statements_;
            qi::rule<Iterator, ast::statement_list(), skip_grammer_type> function_body_block_, function_body_expression_;

            qi::rule<Iterator, ast::return_statement_ptr(), skip_grammer_type> return_statement_;
            qi::rule<Iterator, ast::function_definition_statement_ptr(), skip_grammer_type> function_definition_statement_;
            qi::rule<Iterator, ast::expression_statement_ptr(), skip_grammer_type> expression_statement_;
            qi::rule<Iterator, ast::extern_statement_base_ptr(), skip_grammer_type> extern_statement_;
            qi::rule<Iterator, ast::extern_function_declaration_statement_ptr(), skip_grammer_type> extern_function_declaration_statement_;
            qi::rule<Iterator, ast::empty_statement_ptr(), skip_grammer_type> empty_statement_;

            qi::rule<Iterator, ast::variable_declaration(), skip_grammer_type> variable_declaration_;
            qi::rule<Iterator, ast::variable_declaration(), skip_grammer_type> parameter_variable_declaration_;

            qi::rule<Iterator, ast::parameter_list(), skip_grammer_type> parameter_variable_declaration_list_;

            qi::rule<Iterator, ast::variable_declaration_unit(), skip_grammer_type> variable_initializer_unit_;
            qi::rule<Iterator, ast::variable_declaration_unit(), skip_grammer_type> variable_parameter_initializer_unit_;

            qi::rule<Iterator, ast::value_initializer_unit(), skip_grammer_type> value_initializer_unit_;

            qi::rule<Iterator, ast::type_expression_ptr(), skip_grammer_type> type_specifier_;

            static std::size_t const ExpressionHierarchyNum = 4;
            qi::rule<Iterator, ast::expression_ptr(), skip_grammer_type> expression_, expression_priority_[ExpressionHierarchyNum];
            qi::rule<Iterator, ast::expression_list(), skip_grammer_type> argument_list_;
            qi::rule<Iterator, ast::call_expression_ptr(), skip_grammer_type> call_expression_;
            qi::rule<Iterator, ast::term_expression_ptr(), skip_grammer_type> term_expression_;


            qi::rule<Iterator, ast::type_expression_ptr(), skip_grammer_type> type_expression_;
            qi::rule<Iterator, ast::type_identifier_expression_ptr(), skip_grammer_type> type_identifier_expression_;
            qi::rule<Iterator, ast::compiletime_return_type_expression_ptr(), skip_grammer_type> compiletime_return_type_expression_;


            qi::rule<Iterator, ast::variable_value_ptr(), skip_grammer_type> variable_value_;

            qi::rule<Iterator, ast::intrinsic_value_ptr(), skip_grammer_type> integer_literal_;
            qi::rule<Iterator, ast::intrinsic_value_ptr(), skip_grammer_type> string_literal_;

            qi::rule<Iterator, ast::intrinsic::identifier_value_ptr(), skip_grammer_type> identifier_;
            qi::rule<Iterator, ast::intrinsic::single_identifier_value_ptr(), skip_grammer_type> single_identifier_;

            qi::rule<Iterator, ast::intrinsic::symbol_value_ptr()> native_symbol_;
            qi::rule<Iterator, ast::native_string_t()> native_symbol_string_;

            qi::rule<Iterator, ast::native_string_t()> string_literal_sequenece_;

            qi::rule<Iterator> statement_termination_;



    
        /*
            qi::rule<input_iterator, statement(), ascii::space_type> top_level_statement_, statement_;
            qi::rule<input_iterator, assignment_statement(), ascii::space_type> assignment_statement_;

            qi::rule<input_iterator, expression(), ascii::space_type> expression_;

            qi::rule<input_iterator, symbol()> holder_, pointer_;

            qi::rule<input_iterator, symbol()> lower_symbol_, upper_symbol_;*/
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_PARSER_HPP*/
