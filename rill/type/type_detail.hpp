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
            using value_ptr = void*;

            type_detail_ptr         element_type_detail;
            union {
                value_ptr           as_value;
                type_detail_ptr     as_type_detail;
            } element;
            dependent_value_kind    kind;
            type_id_t               type_id;

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
                    // assert( false && "not supported" );
                    return false;
                }
            }
        };
        typedef std::vector<dependent_type>             template_args_type;
        typedef std::shared_ptr<template_args_type>     template_args_pointer;

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
            template_args_pointer const& sd = nullptr,
            bool const ix = false,
            evaluate_mode const& em = evaluate_mode::k_everytime
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
