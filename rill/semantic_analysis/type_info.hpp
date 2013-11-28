//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#ifndef RILL_SEMANTIC_ANALYSIS_TYPE_INFO_HPP
#define RILL_SEMANTIC_ANALYSIS_TYPE_INFO_HPP

#include "../ast/value_fwd.hpp"
#include "../ast/expression_fwd.hpp"
#include "../attribute/type.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        struct type_info
        {
            ast::intrinsic::identifier_value_ptr identifier;
            attribute::type_attributes_optional attributes;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_TYPE_INFO_HPP*/
