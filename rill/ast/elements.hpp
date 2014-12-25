//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <vector>

#include <boost/optional.hpp>

#include "../type/attribute.hpp"
#include "../decl/attribute.hpp"

#include "detail/base_type.hpp"
#include "statement_def.ipp"
#include "expression_def.ipp"
#include "value_def.ipp"


namespace rill
{
    namespace ast
    {
        // statemets
        namespace element
        {
            using statement_list = std::vector<statement_ptr>;
        }

        // namespace element {
        //
        struct value_initializer_unit
        {
            value_initializer_unit() = default;

            value_initializer_unit(
                expression_ptr const& ep
                )
                : initializer( ep )
                , type( nullptr )
            {}

            value_initializer_unit(
                id_expression_ptr const& tp,
                boost::optional<expression_ptr> const& ep
                )
                : initializer( ep != boost::none ? *ep : nullptr )
                , type( tp )
            {}

            value_initializer_unit( value_initializer_unit const& rhs )
                : initializer( clone( rhs.initializer ) )
                , type( clone( rhs.type ) )
            {}
            value_initializer_unit& operator=( value_initializer_unit const& ) =default;

            value_initializer_unit( value_initializer_unit&& rhs )
                : initializer( std::move( rhs.initializer ) )
                , type( std::move( rhs.type ) )
            {}
            value_initializer_unit& operator=( value_initializer_unit&& ) =default;

            expression_ptr initializer;
            id_expression_ptr type;
        };


        struct variable_declaration_unit
        {
            variable_declaration_unit() = default;

            variable_declaration_unit(
                const_identifier_value_base_ptr const& n,
                attribute::decl::type const& attr,
                value_initializer_unit&& i
                )
                : name( n )
                , decl_attr( attr )
                , init_unit( std::move( i ) )
            {}

            template<typename T>
            variable_declaration_unit(
                boost::optional<T> const& n,
                attribute::decl::type const& attr,
                value_initializer_unit&& i
                )
                : variable_declaration_unit(
                    n != boost::none ? *n : nullptr,
                    attr,
                    std::move( i )
                    )
            {}

            variable_declaration_unit(
                const_identifier_value_base_ptr const& n,
                attribute::decl::type const& attr,
                boost::optional<value_initializer_unit>&& i
                )
                : variable_declaration_unit(
                    n,
                    attr,
                    i != boost::none ? std::move( *i ) : value_initializer_unit{}
                    )
            {}

            variable_declaration_unit( variable_declaration_unit const& rhs )
                : name( clone( rhs.name ) )
                , decl_attr( rhs.decl_attr )
                , init_unit( rhs.init_unit )
            {}
            variable_declaration_unit& operator=( variable_declaration_unit const& ) =default;

            variable_declaration_unit( variable_declaration_unit&& rhs )
                : name( std::move( rhs.name ) )
                , decl_attr( std::move( rhs.decl_attr ) )
                , init_unit( std::move( rhs.init_unit ) )
            {}
            variable_declaration_unit& operator=( variable_declaration_unit&& ) =default;

            const_identifier_value_base_ptr name;
            attribute::decl::type decl_attr;
            value_initializer_unit init_unit;
        };


        typedef std::vector<variable_declaration_unit> variable_declaration_unit_container_t;


        struct variable_declaration
        {
            attribute::holder_kind quality;        // Ex. val | ref | ...
            variable_declaration_unit decl_unit;
        };

        typedef std::vector<variable_declaration> parameter_list_t;

        // } // namespace element
        namespace element
        {
            struct class_variable_initializers
            {
                variable_declaration_unit_container_t initializers;
            };

        } // namespace element

        // expressions
        typedef std::vector<expression_ptr> expression_list;
        typedef std::vector<id_expression_ptr> id_expression_list;

    } // namespace ast
} // namespace rill
