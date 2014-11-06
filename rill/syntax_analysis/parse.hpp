//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_PARSE_HPP
#define RILL_SYNTAX_ANALYSIS_PARSE_HPP

#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>

#include "../ast.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace spirit = boost::spirit;

        using string_iterator_t
            = ast::native_string_t::const_iterator;

        using iterator_t
            = spirit::line_pos_iterator<string_iterator_t>;

        auto parse(
            boost::filesystem::path const& source_fullpath
            )
            -> ast::module_ptr;

        auto parse(
            ast::native_string_t const& source,
            boost::optional<boost::filesystem::path> const& source_fullpath = boost::none
            )
            -> ast::module_ptr;

        auto parse(
            iterator_t& it,
            string_iterator_t const& orig_begin,
            iterator_t const& end,
            boost::optional<boost::filesystem::path> const& source_fullpath = boost::none
            )
            -> ast::module_ptr;

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_PARSE_HPP*/
