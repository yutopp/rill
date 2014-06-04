#define BOOST_TEST_MODULE syntax_test
#include <boost/test/unit_test.hpp>

#include <rill/syntax_analysis/make_syntax_tree.hpp>

#include <rill/debug/debug.hpp>


BOOST_AUTO_TEST_SUITE( Syntax )


BOOST_AUTO_TEST_CASE( empty_string )
{
    auto const& source = "";

    auto const program
        = rill::syntax_analysis::make_syntax_tree( source );

    BOOST_CHECK( program != nullptr );
}


BOOST_AUTO_TEST_CASE( function_statment )
{
    auto const& source = R"ss(
def f(val a: int) => 5;
)ss";

    auto const program
        = rill::syntax_analysis::make_syntax_tree( source );

    BOOST_CHECK( program != nullptr );

    rill::debug::print_ast( program );
}



BOOST_AUTO_TEST_CASE( semantic )
{

}


BOOST_AUTO_TEST_SUITE_END()
