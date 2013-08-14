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
        auto make_syntax_tree( input_type const& source ) -> std::shared_ptr<ast::root>
            try
        {
            auto first      = source.cbegin();
            auto const last = source.cend();

            code_grammer<input_type, input_iterator> grammer;
            ast::statement_list stmts;

            bool const success = qi::phrase_parse( first, last, grammer, boost::spirit::ascii::space, stmts );
            if ( success ) {
                std::cout << "true => " << ( first == last ) << " (1 is ok)" << std::endl;
            } else {
                std::cout << "false" << std::endl;
            }

            ast::root r = { std::move( stmts ) };
            return std::make_shared<ast::root>( r );
        }
        catch( qi::expectation_failure<input_iterator> const& /*e*/ )
        {
            ast::statement_list p;
            ast::root r = { std::move( p ) /* TODO: insert error*/ };
            return std::make_shared<ast::root>( r );
        }

    } // namespace syntax_analysis
} // namespace rill
