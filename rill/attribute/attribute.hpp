//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ATTRIBUTE_HPP
#define RILL_ATTRIBUTE_HPP

#include <bitset>


namespace rill
{
    namespace attribute
    {
        // TODO: rename -> holder_kind
        enum class quality_kind
        {
            k_suggest = 0,
            k_val,
            k_ref
        };

        enum class modifiability_kind
        {
            k_none = 4,
            k_mutable,
            k_const,
            k_immutable,
        };

        auto const attributes_width_max = 8;
        typedef std::bitset<attributes_width_max> attributes_bit_t;

        struct type_attributes
        {
            attribute::quality_kind quality;
            attribute::modifiability_kind modifiability;
        };


        namespace detail
        {
            inline void set_type_attribute(
                type_attributes& attr,
                quality_kind const& k
                )
            {
                attr.quality = k;
            }

            inline void set_type_attribute(
                type_attributes& attr,
                modifiability_kind const& k
                )
            {
                attr.modifiability = k;
            }

            template<typename T>
            void set(
                type_attributes& attr,
                T const& arg
                )
            {
                set_type_attribute( attr, arg );
            }

            template<typename T, typename... Args>
            void set(
                type_attributes& attr,
                T const& arg,
                Args const&... args
                )
            {
                set_type_attribute( attr, arg );
                set( attr, args... );
            }
        } // namespace detail

        inline auto operator+=(
            type_attributes& a,
            type_attributes const& b
            )
            -> type_attributes&
        {
            detail::set(
                a,
                b.quality,
                b.modifiability
                );

            return a;
        }

        inline auto operator+(
            type_attributes a,  // copy
            type_attributes const& b
            )
            -> type_attributes
        {
            a += b;

            return a;
        }


        namespace detail
        {
            inline void unset_type_attribute(
                type_attributes& attr,
                quality_kind const& k
                )
            {
                if ( k != quality_kind::k_suggest ) {
                    attr.quality = quality_kind::k_suggest;
                }
            }

            inline void unset_type_attribute(
                type_attributes& attr,
                modifiability_kind const& k
                )
            {
                if ( k != modifiability_kind::k_none ) {
                    attr.modifiability = modifiability_kind::k_none;
                }
            }

            template<typename T>
            void unset(
                type_attributes& attr,
                T const& arg
                )
            {
                unset_type_attribute( attr, arg );
            }

            template<typename T, typename... Args>
            void unset(
                type_attributes& attr,
                T const& arg,
                Args const&... args
                )
            {
                unset_type_attribute( attr, arg );
                unset( attr, args... );
            }
        } // namespace detail

        inline auto operator-=(
            type_attributes& a,
            type_attributes const& b
            )
            -> type_attributes&
        {
            detail::unset(
                a,
                b.quality,
                b.modifiability
                );

            return a;
        }

        inline auto operator-(
            type_attributes a,  // copy
            type_attributes const& b
            )
            -> type_attributes
        {
            a -= b;

            return a;
        }

    } // namespace attribute
} // namespace rill

#endif /*RILL_ATTRIBUTE_HPP*/
