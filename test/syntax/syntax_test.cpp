#define BOOST_TEST_MODULE syntax_test
#include <boost/test/unit_test.hpp>

#include <rill/syntax_analysis/parse.hpp>

#define PARSE_PASS( ast ) \
    BOOST_CHECK( ( ast ) != nullptr );

#define PARSE_FAIL( ast ) \
    BOOST_CHECK( ( ast ) == nullptr );


BOOST_AUTO_TEST_SUITE( Syntax )

BOOST_AUTO_TEST_CASE( pass_test_0 )
{
    std::string const s = R"s(; ; /**/;)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_PASS( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_1 )
{
    std::string const s = R"s(def fg; /**/; ;/**/)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_FAIL( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_2 )
{
    std::string const s = R"s(a+a;)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_PASS( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_3 )
{
    std::string const s = R"s(
def test() {
}
)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_PASS( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_4 )
{
    std::string const s = R"s(
def test( val a: int ) {
}
)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_PASS( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_5 )
{
    std::string const s = R"s(
def test( ref a: int ) => a;
)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_PASS( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_6 )
{
    std::string const s = R"s(
deftest( ref a: int ) => a;
)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_FAIL( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_7 )
{
    std::string const s = R"s(
classtest{}
)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_FAIL( ast );
}

BOOST_AUTO_TEST_CASE( pass_test_8 )
{
    std::string const s = R"s(
class test {
}
)s";

    auto const& ast = rill::syntax_analysis::parse( s );
    PARSE_PASS( ast );
}

BOOST_AUTO_TEST_SUITE_END()
