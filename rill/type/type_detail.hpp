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
    struct type_detail;

    using type_detail_ptr = type_detail*;
    using const_type_detail_ptr = type_detail const*;

    struct raw_value_holder
    {
        explicit raw_value_holder(
            std::shared_ptr<void> p,
            environment_base_ptr const& e = nullptr
            )
            : ptr_to_raw_value( std::move( p ) )
            , target_env( e )
            , is_placeholder( false )
        {}

        std::shared_ptr<void> ptr_to_raw_value;
        environment_base_ptr target_env;

        bool is_placeholder;
    };
    using raw_value_holder_ptr = raw_value_holder*;
    using const_raw_value_holder_ptr = raw_value_holder const*;

    enum class dependent_value_kind
    {
        k_type = 0,
        k_alias = 1,/*unused*/

        k_int8 = 4,
        k_int32 = 10,
        k_string = 11,
        k_array = 12,

        k_none = 404
    };

    struct type_detail
    {
        typedef std::vector<type_detail_ptr>        nest_type;
        typedef std::shared_ptr<nest_type>          nest_pointer;

        // dependent_type will holds type_detail_ptr or "TODO: write"
        struct dependent_type
        {
            type_detail_ptr             element_type_detail;
            union {
                void*                   dummy;
                type_detail_ptr         as_type_detail;
                raw_value_holder_ptr    as_value_holder;
            } element;
            dependent_value_kind        kind;
            type_id_t                   type_id;

            inline auto is_type() const
               -> bool
            {
                return kind == dependent_value_kind::k_type;
            }

            inline auto has_placeholder() const
                -> bool
            {
                if ( is_type() ) {
                    return element.as_type_detail->is_placeholder
                        || element.as_type_detail->has_placeholder();

                } else {
                    return element.as_value_holder->is_placeholder;
                }
            }
        };
        typedef std::vector<dependent_type>             template_args_type;
        typedef std::shared_ptr<template_args_type>     template_args_pointer;

        enum class evaluate_mode
        {
            k_only_runtime,
            k_runtime,
            k_meta,
            k_only_meta,
        };

        explicit type_detail(
            type_id_t const& w,
            environment_base_ptr const& e,
            nest_pointer const& st = nullptr,
            template_args_pointer const& sd = nullptr,
            bool const ix = false,
            evaluate_mode const& em = evaluate_mode::k_runtime
            )
            : type_id( w )
            , target_env( e )
            , nest( st )
            , template_args( sd )
            , is_xvalue( ix )
            , eval_mode( em )
            , is_placeholder( false )
        {}

        type_id_t type_id;
        environment_base_ptr target_env;
        nest_pointer nest;
        template_args_pointer template_args;

        bool is_xvalue;
        evaluate_mode eval_mode;
        bool is_placeholder;

    public:
        inline auto has_template_args() const
            -> bool
        {
            return template_args != nullptr;
        }

        inline auto has_placeholder() const
            -> bool
        {
            if ( template_args ) {
                bool has_placeholder = false;
                for( auto&& arg : *template_args ) {
                    has_placeholder |= arg.has_placeholder();
                }
                return has_placeholder;
            }

            return false;
        }
    };

} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_TYPE_DETAIL_HPP*/
