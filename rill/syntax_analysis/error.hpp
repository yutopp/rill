//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_ERROR_HPP
#define RILL_SYNTAX_ANALYSIS_ERROR_HPP

#include <memory>
#include <vector>

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3 1
#endif
#include <boost/spirit/include/qi.hpp>

#include "types.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        class error_info
        {
        };

        //
        using error_container = std::vector<error_info>;

        //
        auto report_and_repair_status(
            pos_iterator_type& first,
            pos_iterator_type const& end,
            pos_iterator_type const& where,
            boost::spirit::info const& what,
            std::shared_ptr<error_container> const& errors
            ) -> void;

    } // namespace sytax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_ERROR_HPP*/
