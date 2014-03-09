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

#include "../ast/value_fwd.hpp"
#include "../ast/expression_fwd.hpp"
#include "../attribute/type.hpp"
//#include "../compile_time/"

namespace rill
{
    namespace semantic_analysis
    {
        // TODO remove
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


        struct type_detail;

        using type_detail_pool_t = boost::object_pool_workarounded<type_detail>;
        using type_detail_ptr = type_detail*;

        // TODO: move to any where
        enum class value_kind_mask : char
        {
            k_value = 0,
            k_type = 1,
            k_alias = 2/*unused*/
        };
        // TODO: move to any where

        struct type_detail
        {
            typedef std::vector<type_detail_ptr>        nest_type;
            typedef std::shared_ptr<nest_type>          nest_pointer;

            // dependent_type will holds type_detail_ptr or llvm::Value* in "element"
            // if "element_class_env" holds "type" class env, "element" holds type_detail_ptr value.
            // otherwise, "elements" holds llvm::Value*
            struct dependent_type
            {
                const_class_symbol_environment_ptr  element_class_env;
                void*                               element;
                value_kind_mask                     kind;

                inline auto const is_type() const
                   -> bool
                {
                    return kind == value_kind_mask::k_type;
                }
            };
            typedef std::vector<dependent_type>         template_arg_type;
            typedef std::shared_ptr<template_arg_type>  template_arg_pointer;

            explicit
            type_detail(
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
