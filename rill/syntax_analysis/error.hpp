//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_ERROR_HPP
#define RILL_SYNTAX_ANALYSIS_ERROR_HPP

#include <iostream>
#include <iomanip>
#include <string>
#include <memory>

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3
#endif

#include <boost/spirit/include/support_line_pos_iterator.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        namespace spirit = boost::spirit;
        namespace qi = spirit::qi;

        namespace detail
        {
            template <class Iterator>
            inline Iterator get_line_end(
                Iterator current,
                Iterator const& upper_bound
                )
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

            template <class Iterator>
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
                ++first;
      
                return boost::iterator_range<Iterator>( first, last );
            }
        } // namespace detail


        template<typename It>
        class error_handler_lazy
        {
        public:
            typedef void    result_type;

        public:
            template<typename T>
            auto operator()(
                It const& first,
                It const& end,
                It const& where,
                T const& what
                ) const
                -> result_type
            {
                std::cerr
                    << "Error: expecting "
                    << what
                    << " in line "
                    << spirit::get_line( where )
                    << " position "
                    << spirit::get_column( first, where )
                    << ": \n"
                    << detail::get_current_line_range( first, where, end )
                    << "\n"
                    << std::string( get_column( first, where ), ' ' )
                    << "^ here"
                    << std::endl
                    ;
            }
        };

    } // namespace sytax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_ERROR_HPP*/
