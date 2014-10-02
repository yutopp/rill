//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_SKIP_GRAMMAR_HPP
#define RILL_SYNTAX_ANALYSIS_SKIP_GRAMMAR_HPP

#include <boost/spirit/home/x3.hpp>

#include "helper.hpp"


#define R   RILL_RULE
#define RC  RILL_RULE_WITH_ANNOTATOR

namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        namespace skip_grammer
        {
            RILL_RULES_BEGIN( rules, skipper )

            R( skipper, x3::unused_type,
                x3::space /*includes newlines*/
                | ( "//" >> *( x3::char_ - '\n' ) >> ( '\n' | x3::eoi ) )
                | ( "/*" >> *( x3::char_ - "*/" ) >> "*/" )
                )
            RILL_RULES_END

        } // namespace skipper
    } // namespace syntax_analysis
} // namespace rill

#undef R
#undef RC

#endif /*RILL_SYNTAX_ANALYSIS_SKIP_GRAMMAR_HPP*/
