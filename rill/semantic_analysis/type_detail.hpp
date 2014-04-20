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

//#include <boost/pool/object_pool.hpp>
#include "work_around_object_pool.hpp"

#include "../environment/type_registry_fwd.hpp"

#include "../ast/value_fwd.hpp"
#include "../ast/expression_fwd.hpp"
#include "../attribute/type.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        struct type_detail;

        using type_detail_pool_t = boost::object_pool_workarounded<type_detail>;
        using type_detail_ptr = type_detail*;

        // TODO: move to any where
        enum class dependent_value_kind
        {
            k_type = 0,
            k_alias = 1,/*unused*/

                k_int32 = 10,
                k_string = 11,
                k_array = 12,

                k_none = 404
        };
        // TODO: move to any where

        struct type_detail
        {
            typedef std::vector<type_detail_ptr>        nest_type;
            typedef std::shared_ptr<nest_type>          nest_pointer;

            // dependent_type will holds type_detail_ptr or "TODO: write"
            struct dependent_type
            {
                type_detail_ptr         element_type_detail;
                void*                   element;
                dependent_value_kind    kind;

                inline auto is_type() const
                   -> bool
                {
                    return kind == dependent_value_kind::k_type;
                }
            };
            typedef std::vector<dependent_type>         template_arg_type;
            typedef std::shared_ptr<template_arg_type>  template_arg_pointer;

            explicit type_detail(
                type_id_t const& w,
                environment_base_ptr const& e,
                nest_pointer const& st = nullptr,
                template_arg_pointer const& sd = nullptr
                )
                : type_id( w )
                , target_env( e )
                , nest( st )
                , template_args( sd )
            {}

            type_id_t type_id;
            environment_base_ptr target_env;
            nest_pointer nest;
            template_arg_pointer template_args;

        public:
            inline auto has_template_args() const
                -> bool
            {
                return template_args != nullptr;
            }
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_TYPE_INFO_HPP*/
