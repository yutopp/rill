//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once


#include <cassert>
#include <memory>
#include <unordered_map>
#include <bitset>
#include <vector>
#include <utility>
#include <boost/range/adaptor/transformed.hpp>

#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"


namespace rill
{
    //
    // class
    //
    class class_symbol_environment RILL_CXX11_FINAL
        : public single_identifier_environment_base
    {
    public:
        static kind::type_value const KindValue = kind::type_value::e_class;

    public:
        class_symbol_environment( environment_parameter_t&& pp )
            : single_identifier_environment_base( std::move( pp ) )
            //, kind_( kind )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
        }

        auto complete(
            native_string_type const& name
            )
            -> void
        {
            name_ = name;

            change_progress_to_completed();
        }
        auto mangled_name() const
            -> native_string_type;

        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "class_symbol_environment" << std::endl;
            return dump_include_env( os, indent );
        }

        auto make_type_id_from(
            attribute::type_attributes const& type_attr
                = attribute::make_default_type_attributes()
            ) const
            -> shared_resource_type::type_registry_type::type_id_type
        {
            return make_type_id( get_id(), type_attr );
        }

    private:

        native_string_type name_;
    };

} // namespace rill
