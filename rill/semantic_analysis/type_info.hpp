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

#include <memory>
#include <vector>

#include "../ast/value_fwd.hpp"
#include "../ast/expression_fwd.hpp"
#include "../attribute/type.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        struct type_info
        {
            ast::nested_identifier_value_ptr identifiers;
            attribute::type_attributes_optional attributes;
        };


        // TODO: move to any where
        struct type_id_with_env
        {
            type_id_t type_id;
            environment_base_ptr target_env;
            std::shared_ptr<std::vector<type_id_with_env>> nest;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_TYPE_INFO_HPP*/
