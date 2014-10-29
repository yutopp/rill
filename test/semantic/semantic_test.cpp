#define BOOST_TEST_MODULE semantic_test
#include <boost/test/unit_test.hpp>

#include <rill/syntax_analysis/parse.hpp>

#define PASS( ast ) \
    BOOST_CHECK( ( ast ) != nullptr );

#define FAIL( ast ) \
    BOOST_CHECK( ( ast ) == nullptr );


BOOST_AUTO_TEST_SUITE( Semantic )

BOOST_AUTO_TEST_SUITE_END()
