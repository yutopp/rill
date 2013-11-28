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
//#include <unordered_map>
//#include <bitset>
//#include <vector>
//#include <utility>
//#include <boost/range/adaptor/transformed.hpp>

//#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"


namespace rill
{
    //
    // variable
    //
    class variable_symbol_environment RILL_CXX11_FINAL
        : public single_identifier_environment_base
    {
    public:
        static kind::type_value const KindValue = kind::type_value::variable_e;

    public:
        variable_symbol_environment( environment_id_t const& id, weak_env_base_pointer const& parent )
            : single_identifier_environment_base( id, parent )
            , value_type_id_( type_id_undefined )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
        }

        auto is_incomplete() const
            -> bool RILL_CXX11_OVERRIDE
        {
            return true;//value_type_env_id_ == envitonment_id_undefined;
        }

        auto complete(
            type_id_t const& type_id,
            native_string_type const& name
            )
            -> void
        {
            value_type_id_ = type_id;
            name_ = name;
        }

/*
        // ?!?!!????
        auto get_type_env_id() const
            -> environment_id_t
        {
            return value_type_env_id_;
        }

        auto get_type_environment_id() const
            -> environment_id_t
        {
            return value_type_env_id_;
        }

        auto get_type_environment()
            -> class_symbol_environment_ptr
        {
            auto const& p = get_env_at( get_type_environment_id() );

            return std::dynamic_pointer_cast<class_symbol_environment>( p.lock() );
        }

        auto get_type_environment() const
            -> const_class_symbol_environment_ptr
        {
            auto const& p = get_env_at( value_type_env_id_ );

            return std::dynamic_pointer_cast<class_symbol_environment const>( p.lock() );
        }

        auto get_type_attributes() const
            -> attribute::type_attributes const&
        {
            return value_type_attributes_;
        }
*/
        auto get_type_id() const
            -> type_id_t
        {
            return value_type_id_;
        }

        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os << indent << "varialbe_environment" << std::endl;
    //        os << indent << "  = return type  " << value_type_env_id_ << std::endl;
            return dump_include_env( os, indent );
        }

        auto mangled_name() const -> native_string_type
        {
            // TODO: call parent mangled_name()
            return name_;
        }

    private:
        type_id_t value_type_id_;

        native_string_type name_;
    };

} // namespace rill
