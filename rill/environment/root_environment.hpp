//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <boost/filesystem/path.hpp>
#include "environment_base.hpp"


namespace rill
{
    class namespace_environment
        : public environment_base
    {
    public:
        static kind::type_value const KindValue;

    public:
        namespace_environment( environment_parameter_t&& pp )
            : environment_base( std::move( pp ) )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value override final
        {
            return KindValue;
        }
    };

    class module_environment final
        : public namespace_environment
    {
    public:
        module_environment(
            environment_parameter_t&& pp,
            boost::filesystem::path const& filepath
            )
            : namespace_environment( std::move( pp ) )
            , filepath_( filepath )
        {}

    public:
        auto get_filepath() const
            -> boost::filesystem::path const&
        {
            return filepath_;
        }

    private:
        boost::filesystem::path filepath_;
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
