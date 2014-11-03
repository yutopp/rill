//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_DECL_ATTRIBUTE_HPP
#define RILL_DECL_ATTRIBUTE_HPP

namespace rill
{
    namespace attribute
    {
        namespace decl
        {
            using type = int;

            constexpr type k_default    = 0;
            constexpr type k_onlymeta   = 1 << 0;
            constexpr type k_intrinsic  = 1 << 1;
            constexpr type k_extern     = 1 << 2;
        }

    } // namespace attribute
} // namespace rill

#endif /*RILL_DECL_ATTRIBUTE_HPP*/
