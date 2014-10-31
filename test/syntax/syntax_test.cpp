#define BOOST_TEST_MODULE syntax_test
#include <boost/test/unit_test.hpp>

#include <rill/syntax_analysis/parse.hpp>

#define PASS( src )                                         \
    std::string const s = src;                              \
    auto const& ast = rill::syntax_analysis::parse( s );    \
    BOOST_CHECK( ast != nullptr );                          \

#define FAIL( src )                                         \
    std::string const s = src;                              \
    auto const& ast = rill::syntax_analysis::parse( s );    \
    BOOST_CHECK( ast == nullptr );                          \


BOOST_AUTO_TEST_SUITE( Syntax )

BOOST_AUTO_TEST_CASE( pass_test_0 )
{
    PASS( R"s(; ; /**/;)s" )
}

BOOST_AUTO_TEST_CASE( pass_test_1 )
{
    PASS( R"s(a+a;)s" )
}

BOOST_AUTO_TEST_CASE( pass_test_2 )
{
    PASS( R"s(
def test() {
}
)s" )
}

BOOST_AUTO_TEST_CASE( pass_test_3 )
{
    PASS( R"s(
def test( val a: int ) {
}
)s" )
}

BOOST_AUTO_TEST_CASE( pass_test_4 )
{
    PASS( R"s(
def test( ref a: int ) => a;
)s" )
}

BOOST_AUTO_TEST_CASE( pass_test_5 )
{
    PASS( R"s(
class test {
}
)s" )
}

BOOST_AUTO_TEST_CASE( pass_test_6 )
{
    PASS( R"s(
def main(): int
{
    val a = HogeHuga!(int, int)();
    val b: HogeHuga!(int, string) = HogeHuga!(int, string)();
    val c: HogeHuga!(void, string) = HogeHuga!(void, string)();

    return 0;
}

template(T: type, U: type)
class HogeHuga
{
    def ctor()
    {
    }

    def f(): void
    {
        this.a;
    }

    val b: 32;
    val a: T;
}
)s" )
}



BOOST_AUTO_TEST_CASE( fail_test_0 )
{
    FAIL( R"s(def fg; /**/; ;/**/)s" )
}

BOOST_AUTO_TEST_CASE( fail_test_1 )
{
    FAIL( R"s(
deftest( ref a: int ) => a;
)s" )
}

BOOST_AUTO_TEST_CASE( fail_test_2 )
{
    FAIL( R"s(
classtest{}
)s" )
}

BOOST_AUTO_TEST_CASE( fail_test_3 )
{
    FAIL( R"s(
deftest( ref a: int ) => a;
deftest( ref a: int ) => a;
deftest( ref a: int ) => a;
)s" )
}

BOOST_AUTO_TEST_CASE( fail_test_4 )
{
    FAIL( R"s(
def main(): int
{
    val a = HogeHuga!(int, int)();
    val b: HogeHuga!(int, string) = HogeHuga!(int, string)();
    val c: HogeHuga!(void, string) = HogeHuga!(void, string)();

    return 0;
}

template(T: type, U: type)
class HogeHuga
{
    def ctor()
    {
    }

    def f(): void
    {
        this.a;b
    }

    val b: 32;
    val a: T;
}
)s" )
}

BOOST_AUTO_TEST_SUITE_END()
