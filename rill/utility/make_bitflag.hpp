//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_UTILITY_MAKE_BITFLAG_HPP
#define RILL_UTILITY_MAKE_BITFLAG_HPP

#include <type_traits>


#define RILL_MAKE_ENUM_TO_BITFLAG( ty )                         \
    inline auto operator|( ty const& lhs, ty const& rhs )       \
        -> ty                                                   \
    {                                                           \
        typedef std::underlying_type<ty>::type      ut;         \
        return static_cast<ty>(                                 \
            static_cast<ut>( lhs ) | static_cast<ut>( rhs )     \
            );                                                  \
    }                                                           \
    inline auto operator&( ty const& lhs, ty const& rhs )       \
        -> ty                                                   \
    {                                                           \
        typedef std::underlying_type<ty>::type      ut;         \
        return static_cast<ty>(                                 \
            static_cast<ut>( lhs ) & static_cast<ut>( rhs )     \
            );                                                  \
    }                                                           \

#endif /*RILL_UTILITY_MAKE_BITFLAG_HPP*/
