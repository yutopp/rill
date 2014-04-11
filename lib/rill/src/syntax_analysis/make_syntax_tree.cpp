//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/syntax_analysis/make_syntax_tree.hpp>
#include <rill/syntax_analysis/code_grammer.hpp>
#include <rill/syntax_analysis/types.hpp>
#include <rill/syntax_analysis/error.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        auto make_syntax_tree( ast::native_string_t const& source )
            -> ast::statements_ptr
        {
            namespace spirit = boost::spirit;
            namespace qi = spirit::qi;

            using grammer_type = code_grammer<pos_iterator_type>;

            try
            {
                auto const first = pos_iterator_type( source.cbegin() );
                auto const last  = pos_iterator_type( source.cend() );
                auto it = first;

                auto const& ec = std::make_shared<error_container>();

                grammer_type grammer( grammer_type::attacher_type( first, ec ) );
                grammer_type::skip_grammer_type skipper;

                ast::statements_ptr program;

                std::cout << "!!! === !!!" << std::endl
                          << "Start to parse" << std::endl
                          << "===========" << std::endl;

                bool const success = qi::phrase_parse( it, last, grammer, skipper, program );
                if ( success ) {
                    bool const is_parsed_to_end = it == last;
                    bool const has_no_errors = ec->size() == 0;

                    if ( !( is_parsed_to_end && has_no_errors ) ) {
                        std::cout << "=> is_parsed_to_end = " << is_parsed_to_end << std::endl
                                  << "   has_no_errors = " << has_no_errors << std::endl;
                        // Test
                        std::exit( -1 );
                    }
                } else {
                    std::cout << "=> success == false" << std::endl;
                    std::exit( -1 );
                }

                return program;
            }
            catch( qi::expectation_failure<pos_iterator_type> const& e )
            {
                std::cout << "Exception handled. " << e.what() << std::endl;
                std::exit( -1 );

                return nullptr; // TODO: insert error
            }
        }

    } // namespace syntax_analysis
} // namespace rill
