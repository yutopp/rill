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
#include <cctype>
#include <iterator>

#include <boost/spirit/home/x3.hpp>

#include "position.hpp"
#include "error.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        namespace detail
        {
            template<typename Iterator>
            auto skip_error_token( Iterator& it, Iterator const& last )
            {
                if ( it != last ) {
                    std::advance( it, 1 );
                }
                x3::parse( it, last, *( x3::char_ - ';' - '{' - '}' ) );
                x3::parse( it, last, *x3::space );
            }

        } // namespace detail

        class on_error_annotator_base
        {
        public:
            template<typename Iterator, typename Exception, typename Context>
            auto on_error(
                Iterator& it,
                Iterator const& last,
                Exception const& what,
                Context const& context
                )
                -> x3::error_handler_result
            {
                auto const& orig_begin
                    = x3::get<error_iterator_orig_begin_tag>( context );
                auto const first
                    = Iterator( detail::get_line_start( orig_begin, what.where().base() ) );

                auto const column = spirit::get_column( first, what.where() );
                assert( column > 0 );
                auto const errors_space_num = column - 1;

                std::cerr
                    << "Error: expecting "
                    << what.which()
                    << " in line "
                    << spirit::get_line( what.where() )
                    << " position "
                    << column
                    << ": \n"
                    << detail::get_current_line_range( first, what.where(), last )
                    << "\n"
                    << std::string( errors_space_num, ' ' )
                    << "^ here"
                    << std::endl
                    ;

                //
                detail::skip_error_token( it, last );

                // report error
                auto& error_holder
                    = x3::get<error_container_tag>( context ).get();

                // TODO: fix
                error_holder.push_back( 0/*dummy*/ );

                return x3::error_handler_result::retry;
                //return x3::error_handler_result::accept;
            }
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_ON_ERROR_HPP*/
