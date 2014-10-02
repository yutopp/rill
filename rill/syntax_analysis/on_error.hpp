//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_ON_ERROR_HPP
#define RILL_SYNTAX_ANALYSIS_ON_ERROR_HPP

#include <iostream>
#include <boost/spirit/home/x3.hpp>

#include "position.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        namespace detail
        {
            template<typename Iterator>
            auto skip_error_token( Iterator& first, Iterator const& last )
            {
                x3::parse( first, last, *( x3::char_ - x3::space ) );
                x3::parse( first, last, *x3::space );
            }
        } // namespace detail


        class on_error_annotator_base
        {
        public:
            template<typename Iterator, typename Exception, typename Context>
            auto on_error(
                Iterator& first,
                Iterator const& last,
                Exception const& what,
                Context const& context
                )
                -> x3::error_handler_result
            {
                std::cerr
                    << "Error: expecting "
                    << what.which()
                    << " in line "
                    << spirit::get_line( what.where() )
                    << " position "
                    << spirit::get_column( first, what.where() )
                    << ": \n"
                    << detail::get_current_line_range( first, what.where(), last )
                    << "\n"
                    << std::string( get_column( first, what.where() ) - 1, ' ' )
                    << "^ here"
                    << std::endl
                    ;

                detail::skip_error_token( first, last );

                return x3::error_handler_result::retry;
            }
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_ON_ERROR_HPP*/
