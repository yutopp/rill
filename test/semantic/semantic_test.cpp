#define BOOST_TEST_MODULE semantic_test
#include <boost/test/unit_test.hpp>

#include <rill/rill.hpp>
#include <rill/syntax_analysis/parse.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

#define PASS( src )                                                     \
    /* create default rill world */                                     \
    auto const& t = rill::create_world<>();                             \
                                                                        \
    auto const root_env = std::get<0>( t );                             \
    auto const intrinsic_function_action = std::get<1>( t );            \
    std::string const s = src;                                          \
    auto const& ast = rill::syntax_analysis::parse( s );                \
    BOOST_CHECK( ast != nullptr );                                      \
    rill::semantic_analysis::analyse_and_complement(                    \
        root_env,                                                       \
        intrinsic_function_action,                                      \
        ast                                                             \
        );

#define FAIL( src )


BOOST_AUTO_TEST_SUITE( Semantic )

BOOST_AUTO_TEST_CASE( pass_test_0a )
{
    auto a = rill::attribute::make(
        rill::attribute::modifiability_kind::k_immutable
        );

    auto b = rill::attribute::type_attributes{
        rill::attribute::holder_kind::k_suggest,
        rill::attribute::modifiability_kind::k_immutable,
        rill::attribute::lifetime_kind::k_scoped,
    };

    BOOST_CHECK( a == b );
}

BOOST_AUTO_TEST_CASE( pass_test_0 )
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

    val b: U;
    val a: T;
}
)s" )
}

BOOST_AUTO_TEST_SUITE_END()
