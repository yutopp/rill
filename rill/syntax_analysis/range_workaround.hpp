//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_X3_RANGE_WORKAROUND_HPP
#define RILL_SYNTAX_ANALYSIS_X3_RANGE_WORKAROUND_HPP

#include <boost/spirit/home/x3.hpp>


namespace boost { namespace spirit { namespace x3 {
    // work around
    template <typename Encoding, typename Attribute = typename Encoding::char_type>
    struct range_char : char_parser<range_char<Encoding, Attribute>>
    {
        typedef typename Encoding::char_type char_type;
        typedef Encoding encoding;
        typedef Attribute attribute_type;
        static bool const has_attribute =
            !is_same<unused_type, attribute_type>::value;

        template <typename Char>
        range_char(Char begin, Char end)
            : begin_(static_cast<char_type>(begin))
            , end_(static_cast<char_type>(end))
        {}

        template <typename Char, typename Context>
        bool test(Char ch, Context const&) const
        {
            return ((sizeof(Char) <= sizeof(char_type)) || encoding::ischar(ch))
                && ( char_type(ch) >= begin_ && char_type(ch) <= end_ );
        }

        char_type begin_, end_;
    };


    template <typename Encoding, typename Attribute>
    struct get_info<range_char<Encoding, Attribute>>
    {
        typedef std::string result_type;
        std::string operator()(range_char<Encoding, Attribute> const& p) const
        {
            return "[\'" + to_utf8(Encoding::toucs4(p.begin_)) + "\'"
                + ", \'" + to_utf8(Encoding::toucs4(p.end_)) + "\']";
        }
    };
}}}

#endif /*RILL_SYNTAX_ANALYSIS_X3_RANGE_WORKAROUND_HPP*/
