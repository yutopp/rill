//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "environment_base.hpp"


namespace rill
{
    class namespace_environment RILL_CXX11_FINAL
        : public environment_base
    {
    public:
        namespace_environment( environment_parameter_t&& pp )
            : environment_base( std::move( pp ) )
        {}

    private:
        auto get_symbol_kind() const
            -> kind::type_value override final
        {
            return kind::type_value::e_namespace;
        }

    private:

    };

    class alias_environment final
        : public environment_unit
    {
    public:
        alias_environment(
            environment_parameter_t&& pp,
            environment_unit_ptr const& ref
            )
            : environment_unit( std::move( pp ) )
            , reference_env_( ref )
        {}

        auto get_symbol_kind() const
            -> kind::type_value override final
        {
            return kind::type_value::e_alias;
        }

        auto get_reference()
            -> environment_unit_ptr
        {
            return reference_env_;
        }

        auto get_reference() const
            -> const_environment_unit_ptr
        {
            return reference_env_;
        }

    private:
        environment_unit_ptr reference_env_;
    };

} // namespace rill
