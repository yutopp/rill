//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_ON_SUCCESS_HPP
#define RILL_SYNTAX_ANALYSIS_ON_SUCCESS_HPP

#include <iostream>
#include <type_traits>

#include <boost/spirit/home/x3.hpp>

#include "position.hpp"
#include "tag.hpp"
#include "../ast/ast_base.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        namespace x3 = boost::spirit::x3;

        class on_success_annotator_base
        {
        public:
#if 0
            template<typename Iterator, typename Attr, typename Context>
            inline auto on_success(
                Iterator const& first,
                Iterator const& last,
                Attr& attr,
                Context const& context
                ) const
                -> void
            {
                debug_out << "on_success <> " << typeid(Attr).name() << std::endl;
            }
#endif
        };

    } // namespace syntax_analysis
} // namespace rill

#endif /*RILL_SYNTAX_ANALYSIS_ON_SUCCESS_HPP*/
