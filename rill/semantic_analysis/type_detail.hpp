//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#ifndef RILL_SEMANTIC_ANALYSIS_TYPE_DETAIL_HPP
#define RILL_SEMANTIC_ANALYSIS_TYPE_DETAIL_HPP

#include <memory>
#include <vector>

#include "type_detail_pool_t.hpp"

#include "../ast/value_fwd.hpp"
#include "../ast/expression_fwd.hpp"
#include "../type/type.hpp"
#include "../type/type_registry_fwd.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        struct type_detail;

        using type_detail_ptr = type_detail*;
        using const_type_detail_ptr = type_detail const*;

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
                type_id_t               type_id;

                inline auto is_type() const
                   -> bool
                {
                    return kind == dependent_value_kind::k_type;
                }
            };
            typedef std::vector<dependent_type>         template_arg_type;
            typedef std::shared_ptr<template_arg_type>  template_arg_pointer;

            enum class evaluate_mode
            {
                k_everytime,
                k_only_compiletime,
                k_only_runtime
            };

            explicit type_detail(
                type_id_t const& w,
                environment_base_ptr const& e,
                nest_pointer const& st = nullptr,
                template_arg_pointer const& sd = nullptr,
                bool const ix = false,
                evaluate_mode const& em = evaluate_mode::k_everytime
                )
                : type_id( w )
                , target_env( e )
                , nest( st )
                , template_args( sd )
                , is_xvalue( ix )
                , eval_mode( em )
            {}

            type_id_t type_id;
            environment_base_ptr target_env;
            nest_pointer nest;
            template_arg_pointer template_args;

            bool is_xvalue;
            evaluate_mode eval_mode;

        public:
            inline auto has_template_args() const
                -> bool
            {
                return template_args != nullptr;
            }
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_TYPE_DETAIL_HPP*/
