//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CONFIG_MACROS_HPP
#define RILL_CONFIG_MACROS_HPP

#include <cstddef>
#include <cstdlib>

// if only using GCC(>= 4.8.1) or clang(>=3.1), use specifier.
#ifdef _MSC_VER
// pass
#elif __GNUC__ >= 4 && __GNUC_MINOR__ >= 8 && __GNUC_PATCHLEVEL__ >= 1
# define RILL_MACRO_USE_FINAL_AND_OVERRIDE
#elif __clang_major__ >= 3 && __clang_minor__ >= 1
# define RILL_MACRO_USE_FINAL_AND_OVERRIDE
#endif

//
#ifdef RILL_MACRO_USE_FINAL_AND_OVERRIDE
# define RILL_CXX11_FINAL      final
# define RILL_CXX11_OVERRIDE   override
#else
# define RILL_CXX11_FINAL
# define RILL_CXX11_OVERRIDE
#endif

//
#if __GNUC__ >= 4 && __GNUC_MINOR__ >= 7 && __GNUC_PATCHLEVEL__ >= 0
# define RILL_MAX_ALIGN       alignof( ::max_align_t )
#else
# define RILL_MAX_ALIGN       16/*TODO: fix it*/
#endif


// for debug
#ifdef RILL_DEBUG
# define rill_dregion \
    std::cout << "--- LOG : " << __FILE__ << " / " << __LINE__ << " ----" << std::endl;
# define rill_dout std::cout
# define rill_ice( str ) assert( false && ( str ) )
# define rill_ice_assert( cond ) if ( !(cond) ) { rill_ice( #cond ); }

#else
# define rill_dregion if ( false )
# define rill_dout if ( false ) std::cout
# define rill_ice( str )                            \
    std::cerr << "File: " << __FILE__ << std::endl  \
              << "Line: " << __LINE__ << std::endl  \
              << "[ICE] " << str << std::endl       \
              << std::endl;                         \
    std::abort();

# define rill_ice_assert( cond ) if ( !(cond) ) { rill_ice( #cond ); }

#endif

#endif /*RILL_CONFIG_MACROS_HPP*/
