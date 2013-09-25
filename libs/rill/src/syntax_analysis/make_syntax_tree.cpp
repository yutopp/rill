//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/syntax_analysis/make_syntax_tree.hpp>
#include <rill/syntax_analysis/parser.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        auto make_syntax_tree( input_type const& source ) -> ast::root_ptr
            try
        {
            auto first      = source.cbegin();
            auto const last = source.cend();

            typedef code_grammer<input_type, input_iterator>    grammer_type;
            grammer_type grammer;
            grammer_type::skip_grammer_type skipper;

            ast::statement_list stmts;

            bool const success = qi::phrase_parse( first, last, grammer, skipper, stmts );
            if ( success ) {
                std::cout << "true => " << ( first == last ) << " (1 is ok)" << std::endl;
                if ( first != last ) {
                    // Test
                    { char c; std::cin >> c; }
                    std::exit( -1 );
                }
            } else {
                std::cout << "false" << std::endl;
            }

            return std::make_shared<ast::root>( std::move( stmts ) );
        }
        catch( qi::expectation_failure<input_iterator> const& /*e*/ )
        {
            ast::statement_list p;
            return std::make_shared<ast::root>( std::move( p ) /* TODO: insert error*/ );
        }

    } // namespace syntax_analysis
} // namespace rill
