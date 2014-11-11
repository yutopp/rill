#define BOOST_TEST_MODULE semantic_test
#include <boost/test/unit_test.hpp>

#include <rill/rill.hpp>
#include <rill/syntax_analysis/parse.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

#define PASS( src )                                                     \
    /* create default rill world */                                     \
    auto const& t = rill::create_world<>();                             \
                                                                        \
    auto const global_env = std::get<0>( t );                           \
    auto const intrinsic_function_action = std::get<1>( t );            \
    std::string const s = src;                                          \
    auto const& ast = rill::syntax_analysis::parse( s );                \
    BOOST_CHECK( ast != nullptr );                                      \
    auto report = rill::semantic_analysis::analyse_and_complement(      \
        global_env,                                                     \
        ast,                                                            \
        intrinsic_function_action                                       \
        );                                                              \
    BOOST_CHECK( report->is_errored() == false );

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
    val b: HogeHuga!(int, int8) = HogeHuga!(int, int8)();
    val c: HogeHuga!(ptr!(bool), int8) = HogeHuga!(ptr!(bool), int8)();

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

extern class type intrinsic "type_type";
extern class int8 intrinsic "type_int8";
extern class int intrinsic "type_int32";
extern class float intrinsic "type_float";
extern class void intrinsic "type_void";
extern class bool intrinsic "type_bool";
template(T: type, N: int)
extern class array intrinsic "type_array";
template(T: type)
extern class ptr intrinsic "type_ptr";
)s" )
}

BOOST_AUTO_TEST_SUITE_END()
