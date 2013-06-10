#include <rill/make_syntax_tree.hpp>
#include <rill/parser.hpp>


auto make_syntax_tree( input_type const& source ) -> result
try
{
    auto first      = source.cbegin();
    auto const last = source.cend();

    code_grammer<input_type, input_iterator> grammer;
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
catch( qi::expectation_failure<input_iterator> const& /*e*/ )
{
    result r = {};
    return r;
}