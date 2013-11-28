//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ATTRIBUTE_TYPE_HPP
#define RILL_ATTRIBUTE_TYPE_HPP

#include <cstdint>

#include <boost/optional.hpp>
#include <boost/fusion/include/adapt_struct.hpp>

#include "attribute.hpp"


namespace rill
{
    namespace attribute
    {
        struct type_attributes_optional
        {
            boost::optional<attribute::quality_kind> quality;
            boost::optional<attribute::modifiability_kind> modifiability;
        };


        struct type_attributes
        {
            attribute::quality_kind quality;
            attribute::modifiability_kind modifiability;
        };

        namespace detail
        {
            void inline set_type_attribute(
                type_attributes& attr,
                attribute::quality_kind const k
                )
            {
                attr.quality = k;
            }

            void inline set_type_attribute(
                type_attributes& attr,
                attribute::modifiability_kind const k
                )
            {
                attr.modifiability = k;
            }

            template<typename T>
            void inline set(
                type_attributes& attr,
                T const& arg
                )
            {
                set_type_attribute( attr, arg );
            }

            template<typename T, typename... Args>
            void inline set(
                type_attributes& attr,
                T const& arg,
                Args const&... args
                )
            {
                set_type_attribute( attr, arg );
                set( attr, args... );
            }
        }

        template<typename T>
        auto operator<<=( type_attributes& attr, T const& t )
            -> type_attributes&
        {
            detail::set( attr, t );
            return attr;
        }

        template<typename... Args>
        auto inline make_type_attributes( Args const&... args )
            -> type_attributes
        {
            type_attributes attr;
            detail::set( attr, args... );
            return attr;
        }

        namespace detail
        {
            auto inline make_type_attributes_bit( type_attributes const& attr )
                -> attributes_bit_t
            {
                attributes_bit_t bits;

                std::cout << "$$$$$$$$$$$$$$$" << std::endl
                          << static_cast<std::size_t>( attr.quality ) << std::endl
                          << static_cast<std::size_t>( attr.modifiability ) << std::endl;

                bits.set( static_cast<std::size_t>( attr.quality ) );
                bits.set( static_cast<std::size_t>( attr.modifiability ) );

                return bits;
            }
        } // namespace detail


        auto inline make_default_type_attributes()
            -> type_attributes
        {
            // FIXME
            return { quality_kind::k_val, modifiability_kind::k_immutable };
        }

    } // namespace attributes
} // namespace rill


BOOST_FUSION_ADAPT_STRUCT(
    rill::attribute::type_attributes_optional,
    (boost::optional<rill::attribute::quality_kind>,        quality)
    (boost::optional<rill::attribute::modifiability_kind>,  modifiability)
    )


#endif /*RILL_ATTRIBUTE_TYPE_HPP*/
