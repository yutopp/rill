//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/syntax_analysis/make_syntax_tree.hpp>
#include <rill/syntax_analysis/parser.hpp>

#include <boost/spirit/include/support_line_pos_iterator.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        auto make_syntax_tree( ast::native_string_t const& source ) -> ast::root_ptr
        {
            namespace spirit = boost::spirit;
            namespace qi = spirit::qi;

            typedef spirit::line_pos_iterator<ast::native_string_t::const_iterator> pos_iterator_type;
            typedef code_grammer<pos_iterator_type>                                 grammer_type;

            try
            {
                auto const first = pos_iterator_type( source.cbegin() );
                auto const last  = pos_iterator_type( source.cend() );
                auto it = first;

                grammer_type grammer( first );
                grammer_type::skip_grammer_type skipper;

                ast::statement_list program;

                std::cout << "!!! === !!!" << std::endl
                          << "Start to parse" << std::endl
                          << "===========" << std::endl;

                bool const success = qi::phrase_parse( it, last, grammer, skipper, program );
                if ( success ) {
                    std::cout << "true => " << ( it == last ) << " (1 is ok)" << std::endl;
                    if ( it != last ) {
                        // Test
                        { char c; std::cin >> c; }
                        std::exit( -1 );
                    }
                } else {
                    std::cout << "false" << std::endl;
                }

                return std::make_shared<ast::root>( std::move( program ) );
            }
            catch( qi::expectation_failure<pos_iterator_type> const& e )
            {
                std::cout << "Exception handled. " << e.what() << std::endl;
                std::exit( -1 );
                ast::statement_list p;
                return std::make_shared<ast::root>( std::move( p ) /* TODO: insert error*/ );
            }
        }

    } // namespace syntax_analysis
} // namespace rill
