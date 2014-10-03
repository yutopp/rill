//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SYNTAX_ANALYSIS_ERROR_HPP
#define RILL_SYNTAX_ANALYSIS_ERROR_HPP

#include <vector>


namespace rill
{
    namespace syntax_analysis
    {
        struct error_container_tag;

        // TODO: fix me
        using error_container = std::vector<int/* dummy */>;

    } // namespace sytax_analysis
} // rill

#endif /*RILL_SYNTAX_ANALYSIS_ERROR_HPP*/
