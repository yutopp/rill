//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_TYPES_HPP
#define RILL_SYNTAX_ANALYSIS_TYPES_HPP

#ifndef BOOST_SPIRIT_USE_PHOENIX_V3
# define BOOST_SPIRIT_USE_PHOENIX_V3 1
#endif

#include <boost/spirit/include/support_line_pos_iterator.hpp>

#include "../ast/value.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace spirit = boost::spirit;
        namespace qi = spirit::qi;

        typedef spirit::line_pos_iterator<ast::native_string_t::const_iterator> pos_iterator_type;

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_TYPES_HPP*/
