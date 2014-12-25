//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once


namespace rill
{
    enum class class_builtin_kind
    {
        k_none,

        k_type,

        k_void,

        k_int8,
        k_int16,
        k_int32,
        k_int64,

        k_uint8,
        k_uint16,
        k_uint32,
        k_uint64,

        k_float,
        k_double,

        k_bool,

        k_array,

        k_ptr,
    };

} // namespace rill
