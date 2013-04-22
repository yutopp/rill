#include <boost/spirit/include/qi.hpp>


#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/foreach.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <vector>


namespace fusion = boost::fusion;
namespace phx = boost::phoenix;
namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;


typedef std::string             symbol;

typedef symbol                  expression;



struct assignment_statement
{
    symbol          symbol_name;
    expression      value;
};



BOOST_FUSION_ADAPT_STRUCT(
    assignment_statement,
    ( symbol, symbol_name )
    ( expression, value )
)




typedef assignment_statement    statement;

typedef std::vector<statement>  program;




















typedef std::string                     input_type;
typedef input_type::const_iterator      input_iterator;

class code_grammer
    : public qi::grammar<input_iterator, program(), qi::locals<input_type>, ascii::space_type>
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
        upper_symbol_ %= qi::lexeme[ ascii::char_("A-Z") >> *ascii::alnum ];


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

    qi::rule<input_iterator, statement(), ascii::space_type> top_level_statement_, statement_;
    qi::rule<input_iterator, assignment_statement(), ascii::space_type> assignment_statement_;

    qi::rule<input_iterator, expression(), ascii::space_type> expression_;

    qi::rule<input_iterator, symbol()> holder_, pointer_;

    qi::rule<input_iterator, symbol()> lower_symbol_, upper_symbol_;
};


struct result
{
    program product;
};


auto parse( input_type const& source ) -> result
try
{
    auto first      = source.cbegin();
    auto const last = source.cend();

    code_grammer grammer;
    program p;

    bool const success = qi::phrase_parse(
                            first, last,
                            grammer,
                            boost::spirit::ascii::space,
                            p
                            );
    if ( success ) {
        std::cout << "true => " << ( first == last ) << std::endl;
    } else {
        std::cout << "false" << std::endl;
    }

    result r = { std::move( p ) };
    return r;
}
catch( qi::expectation_failure<input_iterator> const& e )
{
    result r = {};
    return r;
}