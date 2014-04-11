//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/syntax_analysis/error.hpp>
#include <rill/syntax_analysis/position.hpp>


namespace rill
{
    namespace syntax_analysis
    {
        auto report_and_repair_status(
            pos_iterator_type& first,
            pos_iterator_type const& end,
            pos_iterator_type const& where,
            boost::spirit::info const& what,
            std::shared_ptr<error_container> const& errors
            ) -> void
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

            // pass iterator to next token
            boost::spirit::qi::parse( first, end, *(boost::spirit::ascii::char_-boost::spirit::ascii::space) );
            boost::spirit::qi::parse( first, end, *boost::spirit::ascii::space );

            errors->push_back( error_info() );
        }

    } // namespace syntax_analysis
} // namespace rill
