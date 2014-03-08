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
#include "../utility/make_bitflag.hpp"

#include "environment_fwd.hpp"


namespace rill
{
    enum class class_attribute
    {
        none        = 0,
        structed    = 1 << 0,
    };
    RILL_MAKE_ENUM_TO_BITFLAG( class_attribute );


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
            native_string_type const& name,
            class_attribute const& attr = class_attribute::none
            )
            -> void
        {
            name_ = name;
            attribute_ = attr;

            change_progress_to_completed();
        }
        auto mangled_name() const
            -> native_string_type;

        auto attribute() const
            -> class_attribute const&
        {
            return attribute_;
        }

        auto has( class_attribute const& attr ) const
            -> bool
        {
            return ( attribute_ & attr ) != static_cast<class_attribute>( 0 );
        }

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


        auto make_as_array(
            type_id_t const& inner_type_id,
            std::size_t const& elements_num
            )
            -> void
        {
            std::cout << "???" << std::endl;
            array_detail_ = std::make_shared<array_detail>( inner_type_id, elements_num );
        }

        auto is_array() const
            -> bool
        {
            return array_detail_ != nullptr;
        }


    public:
        struct array_detail
        {
            array_detail(
                type_id_t const& i,
                std::size_t const& e
                )
                : inner_type_id( i )
                , elements_num( e )
            {}

            type_id_t inner_type_id;
            std::size_t elements_num;
        };

        auto get_array_detail() const
            -> std::shared_ptr<array_detail const>
        {
            return array_detail_;
        }


    private:
        native_string_type name_;
        class_attribute attribute_;

        std::shared_ptr<array_detail> array_detail_;
    };

} // namespace rill
