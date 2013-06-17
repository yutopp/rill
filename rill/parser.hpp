//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <boost/spirit/include/qi.hpp>


#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>

#include <boost/fusion/include/adapt_struct.hpp>
//#include <boost/variant/recursive_variant.hpp>
//#include <boost/foreach.hpp>

#include <iostream>
//#include <fstream>
#include <string>
#include <vector>
#include <memory>

namespace fusion = boost::fusion;
namespace phx = boost::phoenix;
namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;



#include "value.hpp"
#include "expression.hpp"
#include "statement.hpp"

//#include "environment.hpp"



auto make_binary_operator_tree( expression_ptr const& lhs, native_string_t const& op, expression_ptr const& rhs )
    -> expression_ptr
{
    return std::make_shared<binary_operator_expression>(
            lhs,
            literal::make_binary_operator_identifier( op ),
            rhs
            );
}




template<typename StringT, typename Iterator>
class code_grammer
    : public qi::grammar<Iterator, program(), qi::locals<StringT>, ascii::space_type>
{
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
                | expression_statement_
                )
            ;
        
        function_body_statements_
            %= *( return_statement_
                | expression_statement_
                )
            ;
        //
        //block_ = 

        //top_level_statement_

        return_statement_
            = qi::lit( "return" )
            > ( expression_ > statement_termination_ )[
                qi::_val
                    = phx::construct<return_statement_ptr>(
                        phx::new_<return_statement>(
                            qi::_1
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
        function_definition_statement_
            = ( qi::lit( "def" )
            > identifier_
            > parameter_list_
            > qi::lit( ":" )
            > identifier_
            > ( function_body_block_ | function_body_block_ )
              )[
                qi::_val
                    = phx::construct<function_definition_statement_ptr>(
                        phx::new_<function_definition_statement>(
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
                    = phx::construct<expression_statement_ptr>(
                        phx::new_<expression_statement>(
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
                    = phx::construct<call_expression_ptr>(
                        phx::new_<call_expression>(
                            qi::_1,
                            qi::_2
                            )
                        )
              ]
            ;

        // termination
        term_expression_
            = ( integer_literal_
               )[
                   qi::_val = phx::construct<term_expression_ptr>(
                        phx::new_<term_expression>(
                            qi::_1
                            )
                        )
               ]

            ;


        integer_literal_
            = ( qi::int_ )[
                qi::_val
                    = phx::construct<literal::int32_value_ptr>(
                        phx::new_<literal::int32_value>(
                            qi::_1
                            )
                        )
                
              ];

        //auto p = ( -native_symbol_ )[ phx::if_else( qi::_0, phx::construct<literal::symbol_value_ptr>(), phx::construct<literal::symbol_value_ptr>() )]

/**/
        argument_list_.name( "argument_list" );
        argument_list_
            = qi::lit( '(' ) >> ( expression_ % ',' ) >> qi::lit( ')' )
            ;

        parameter_list_.name( "parameter_list" );
        parameter_list_
            = qi::lit( '(' ) >> ( parameter_pair_ % ',' ) >> qi::lit( ')' )
            ;

        parameter_pair_.name( "parameter_pair" );
        parameter_pair_
           // name( optional )      | type                 | default_initializer( optional )
            = -identifier_ >> qi::lit( ':' ) >> identifier_ >> -( qi::lit( '(' ) > integer_literal_ > qi::lit( ')' ) )
            ;

        identifier_.name( "identifier" );
        identifier_
            = single_identifier_[qi::_val = phx::bind( &literal::make_identifier, qi::_1)]
           ;
            // TODO: should +( single_identifier_ | single_template_identifier_ )

        // instanced_identifier

        // static_identifier_

        single_identifier_
            = native_symbol_string_[
                qi::_val
                    = phx::construct<literal::single_identifier_value_ptr>(
                        phx::new_<literal::single_identifier_value>(
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
                    = phx::construct<literal::symbol_value_ptr>(
                        phx::new_<literal::symbol_value>(
                            qi::_1
                            )
                        )
              ]
            ;

        native_symbol_string_.name( "native_symbol_string" );
        native_symbol_string_
            = qi::lexeme[ ascii::char_( "a-zA-Z" ) >> *ascii::alnum ] // TODO: add '_' charactor
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
                << phx::val( "here: \"" )
                << phx::construct<std::string>( _3, _2 )    // iterators to error-pos, end
                << phx::val( "\"" )
                << std::endl
        );
    }

private:
    qi::rule<input_iterator, program(), qi::locals<input_type>, ascii::space_type> program_;

    qi::rule<input_iterator, statement_list(), ascii::space_type> top_level_statements_, function_body_statements_;
    qi::rule<input_iterator, statement_list(), ascii::space_type> function_body_block_, function_body_expression_;

    qi::rule<input_iterator, return_statement_ptr(), ascii::space_type> return_statement_;
    qi::rule<input_iterator, function_definition_statement_ptr(), ascii::space_type> function_definition_statement_;
    qi::rule<input_iterator, expression_statement_ptr(), ascii::space_type> expression_statement_;

    static std::size_t const ExpressionHierarchyNum = 4;
    qi::rule<input_iterator, expression_ptr(), ascii::space_type> expression_, expression_priority_[ExpressionHierarchyNum];
    qi::rule<input_iterator, expression_list(), ascii::space_type> argument_list_;
    qi::rule<input_iterator, call_expression_ptr(), ascii::space_type> call_expression_;
    qi::rule<input_iterator, term_expression_ptr(), ascii::space_type> term_expression_;

    qi::rule<input_iterator, literal::int32_value_ptr(), ascii::space_type> integer_literal_;

    qi::rule<input_iterator, parameter_list(), ascii::space_type> parameter_list_;
    qi::rule<input_iterator, parameter_pair(), ascii::space_type> parameter_pair_;

    qi::rule<input_iterator, literal::identifier_value_ptr(), ascii::space_type> identifier_;
    qi::rule<input_iterator, literal::single_identifier_value_ptr(), ascii::space_type> single_identifier_;

    qi::rule<input_iterator, literal::symbol_value_ptr()> native_symbol_;
    qi::rule<input_iterator, native_string_t()> native_symbol_string_;

    qi::rule<input_iterator, void()> statement_termination_;

/*
    qi::rule<input_iterator, statement(), ascii::space_type> top_level_statement_, statement_;
    qi::rule<input_iterator, assignment_statement(), ascii::space_type> assignment_statement_;

    qi::rule<input_iterator, expression(), ascii::space_type> expression_;

    qi::rule<input_iterator, symbol()> holder_, pointer_;

    qi::rule<input_iterator, symbol()> lower_symbol_, upper_symbol_;*/
};