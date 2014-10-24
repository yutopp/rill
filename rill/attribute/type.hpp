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
            boost::optional<attribute::holder_kind> quality;
            boost::optional<attribute::modifiability_kind> modifiability;
        };


        template<typename T>
        auto operator<<=( type_attributes& attr, T const& t )
            -> type_attributes&
        {
            detail::set( attr, t );
            return attr;
        }





        auto inline make_default_type_attributes()
            -> type_attributes
        {
            return { holder_kind::k_val, modifiability_kind::k_immutable };
        }

        auto inline make_empty_type_attributes()
            -> type_attributes
        {
            return { holder_kind::k_suggest, modifiability_kind::k_none };
        }

        template<typename... Args>
        auto inline make_type_attributes( Args const&... args )
            -> type_attributes
        {
            type_attributes attr = make_default_type_attributes();
            detail::set( attr, args... );
            return attr;
        }

    } // namespace attributes
} // namespace rill


BOOST_FUSION_ADAPT_STRUCT(
    rill::attribute::type_attributes_optional,
    (boost::optional<rill::attribute::holder_kind>,        quality)
    (boost::optional<rill::attribute::modifiability_kind>,  modifiability)
    )


#endif /*RILL_ATTRIBUTE_TYPE_HPP*/
