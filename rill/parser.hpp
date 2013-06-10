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
//#include <vector>
#include <memory>

namespace fusion = boost::fusion;
namespace phx = boost::phoenix;
namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;



#include "value.hpp"
#include "expression.hpp"
#include "statement.hpp"

//#include "environment.hpp"








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
        program_ %= *top_level_statement_;

        //
        top_level_statement_.name( "top_level_statement" );
        top_level_statement_
            %= ( expression_statement_
               ) > ";" > ( qi::eol | qi::eoi )
            ;

        //
        expression_statement_
            = outer_0_expression_[
                qi::_val
                    = phx::construct<expression_statement_ptr>(
                        phx::new_<expression_statement>(
                            qi::_1
                            )
                        )
              ]
            ;



        outer_0_expression_
            = ( term_expression_ >> "*" >> term_expression_ )[
                qi::_val
                    = phx::construct<binary_expression_ptr>(
                        phx::new_<binary_expression>(
                            qi::_1,
                            phx::bind( &literal::make_binary_operator_identifier,
                                phx::construct<literal::symbol_value_ptr>(
                                    phx::new_<literal::symbol_value>( "*" )
                                    )
                                ),
                            qi::_2
                            )
                        )
              ]
            | term_expression_
            ;

        term_expression_
            = ( integer_literal_ )[
                qi::_val
                    = phx::construct<term_expression_ptr>(
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
        parameter_list_
            = qi::lit( '(' ) >> ( parameter_pair_ % ',' ) >> qi::lit( ')' )
            ;

        parameter_pair_
            = identifier_literal_ >> -identifier_literal_ >> -integer_literal_
            ;

        identifier_literal_
           = simple_identifier_literal_;

        simple_identifier_literal_
            = native_symbol_string_[
                qi::_val
                    = phx::construct<literal::simple_identifier_value_ptr>(
                        phx::new_<literal::simple_identifier_value>(
                            qi::_1
                            )
                        )
              ]
            ;

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

        native_symbol_string_
            = qi::lexeme[ ascii::char_( "a-zA-Z" ) >> *ascii::alnum ]
            ;



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


        //
        qi::on_error<qi::fail>
        (
            program_,
            std::cout
                << phx::val("Error! Expecting ")
                << _4                               // what failed?
                << phx::val(" here: \"")
                << phx::construct<std::string>(_3, _2)   // iterators to error-pos, end
                << phx::val("\"")
                << std::endl
        );
    }

private:
    qi::rule<input_iterator, program(), qi::locals<input_type>, ascii::space_type> program_;

    qi::rule<input_iterator, statement_ptr(), ascii::space_type> top_level_statement_;
    qi::rule<input_iterator, expression_statement_ptr(), ascii::space_type> expression_statement_;

    qi::rule<input_iterator, binary_expression_ptr(), ascii::space_type> outer_0_expression_;
    qi::rule<input_iterator, term_expression_ptr(), ascii::space_type> term_expression_;

    qi::rule<input_iterator, literal::int32_value_ptr(), ascii::space_type> integer_literal_;

    qi::rule<input_iterator, parameter_list(), ascii::space_type> parameter_list_;
    qi::rule<input_iterator, parameter_pair(), ascii::space_type> parameter_pair_;

    qi::rule<input_iterator, literal::identifier_value_ptr(), ascii::space_type> identifier_literal_;
    qi::rule<input_iterator, literal::simple_identifier_value_ptr(), ascii::space_type> simple_identifier_literal_;

    qi::rule<input_iterator, literal::symbol_value_ptr()> native_symbol_;
    qi::rule<input_iterator, native_string_t()> native_symbol_string_;

/*
    qi::rule<input_iterator, statement(), ascii::space_type> top_level_statement_, statement_;
    qi::rule<input_iterator, assignment_statement(), ascii::space_type> assignment_statement_;

    qi::rule<input_iterator, expression(), ascii::space_type> expression_;

    qi::rule<input_iterator, symbol()> holder_, pointer_;

    qi::rule<input_iterator, symbol()> lower_symbol_, upper_symbol_;*/
};