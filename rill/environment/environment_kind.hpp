//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <string>


namespace rill
{
    namespace kind
    {
        struct function_tag {};
        auto const k_function = function_tag();

        struct variable_tag {};
        auto const k_variable = variable_tag();

        struct class_tag {};
        auto const k_class = class_tag();

        struct template_tag {};
        auto const k_template = template_tag();

        struct mixin_tag {};
        auto const k_mixin = mixin_tag();

        struct multiset_tag {};
        auto const k_multiset = multiset_tag();

        struct scope_tag {};
        auto const k_scope = scope_tag();

        enum struct type_value
        {
            e_none,
            e_scope,
            e_namespace,
            e_alias,
            e_multi_set,

            e_function,
            e_class,
            e_variable,
            e_template,

            e_mixin
        };


        template<typename R = std::string>
        auto debug_string( type_value const& e )
            -> R
        {
            switch( e )
            {
            case type_value::e_none:
                return "e_none";
            case type_value::e_scope:
                return "e_scope";
            case type_value::e_namespace:
                return "e_namespace";
            case type_value::e_alias:
                return "e_alias";
            case type_value::e_multi_set:
                return "e_multi_set";
            case type_value::e_function:
                return "e_function";
            case type_value::e_variable:
                return "e_variable";
            case type_value::e_class:
                return "e_class";
            case type_value::e_template:
                return "e_template";
            case type_value::e_mixin:
                return "e_mixin";

            default:
                assert( false );
            }

            return "";
        }
    }

} // namespace rill
