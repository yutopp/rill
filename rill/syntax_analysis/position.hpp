//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_POSITION_HPP
#define RILL_SYNTAX_ANALYSIS_POSITION_HPP

#include <boost/spirit/include/support_line_pos_iterator.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        namespace spirit = boost::spirit;

        namespace detail
        {
            template<typename Iterator>
            auto inline get_line_end(
                Iterator current,
                Iterator const& upper_bound
                )
                -> Iterator
            {
                for( ; current != upper_bound; ++current ) {
                    switch( *current ) {
                    case '\r':
                    case '\n':
                        return current;
                    }
                }

                return current;
            }

            template<typename Iterator>
            auto inline get_current_line_range(
                Iterator const& lower_bound,
                Iterator const& current,
                Iterator const& upper_bound
                )
                -> boost::iterator_range<Iterator>
            {
                Iterator first = spirit::get_line_start( lower_bound, current );
                Iterator last = get_line_end( current, upper_bound );

                // to skip last blank...
                //++first;

                return boost::iterator_range<Iterator>( first, last );
            }

        } // namespace detail
    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_POSITION_HPP*/
