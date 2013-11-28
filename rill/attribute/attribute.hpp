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
        enum class quality_kind
        {
            k_suggest = 0,
            k_val,
            k_ref
        };

        enum class modifiability_kind
        {
            k_mutable = 4,
            k_const,
            k_immutable,
        };


        auto const attributes_width_max = 7;
        typedef std::bitset<attributes_width_max> attributes_bit_t;

    } // namespace attribute
} // namespace rill

#endif /*RILL_ATTRIBUTE_HPP*/
