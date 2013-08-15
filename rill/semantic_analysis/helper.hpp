//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_HELPER_HPP
#define RILL_SEMANTIC_ANALYSIS_HELPER_HPP

#include "../environment_fwd.hpp"
#include "../ast/value_fwd.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        auto lookup_with_instanciation( environment_ptr const&, ast::intrinsic::const_identifier_value_ptr const& )
            -> environment_ptr;

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_HELPER_HPP*/
