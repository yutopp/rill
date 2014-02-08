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
//#include "../compile_time/"

namespace rill
{
    namespace semantic_analysis
    {
        struct type_info
        {
            ast::nested_identifier_value_ptr identifiers;
            attribute::type_attributes_optional attributes;
        };


        class type_id_wrapper
        {
        public:
            type_id_wrapper()
                : tid_( type_id_undefined )
            {}

            /*implicit*/
            type_id_wrapper( type_id_t const& tid )
                : tid_( tid )
            {}

        public:
            friend auto operator==( type_id_wrapper const& lhs, type_id_wrapper const& rhs )
                -> bool
            {
                return lhs.tid_ == rhs.tid_;
            }

            friend auto operator==( type_id_wrapper const& lhs, type_id_t const& rhs )
                -> bool
            {
                return lhs.tid_ == rhs;
            }

            operator type_id_t() const
            {
                return tid_;
            }

        private:
            type_id_t tid_;
        };


        // TODO: move to any where
        // TODO: rename it!
        struct type_id_with_env
        {
            struct dependent_type
            {
                //type_id_with_env type;
                // TODO: implement llvm_jit_value value;
            };
               
            type_id_wrapper type_id;
            environment_base_ptr target_env;
            std::shared_ptr<std::vector<type_id_with_env>> nest;
            std::shared_ptr<std::vector<dependent_type>> template_args;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_TYPE_INFO_HPP*/
