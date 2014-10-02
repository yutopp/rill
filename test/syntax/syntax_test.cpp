#define BOOST_TEST_MODULE syntax_test
#include <boost/test/unit_test.hpp>

#include <rill/syntax_analysis/parse.hpp>


BOOST_AUTO_TEST_SUITE( Syntax )


BOOST_AUTO_TEST_CASE( empty_string )
{
    std::string const s = R"s(; ; /**/;)s";
    rill::syntax_analysis::parse( s );

    {
        std::string const s = R"s(def fg; /**/; ;/**/)s";
        rill::syntax_analysis::parse( s );
    }

    {
        std::string const s = R"s(a+a;)s";
        rill::syntax_analysis::parse( s );
    }
}


BOOST_AUTO_TEST_CASE( function_statment )
{
}



BOOST_AUTO_TEST_CASE( semantic )
{

}


BOOST_AUTO_TEST_SUITE_END()
