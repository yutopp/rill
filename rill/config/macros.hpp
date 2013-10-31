//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CONFIG_MACROS_HPP
#define RILL_CONFIG_MACROS_HPP

// if only using GCC(>= 4.8.1) or clang(>=3.1), use specifier.
#ifndef _MSC_VER
# define RILL_MACRO_USE_FINAL_AND_OVERRIDE
#elif __GNUC__ >= 4 && __GNUC_MINOR__ >= 8 && __GNUC_PATCHLEVEL__ >= 1
# define RILL_MACRO_USE_FINAL_AND_OVERRIDE
#elif __clang_major__ >= 3 && __clang_minor__ >= 1
# define RILL_MACRO_USE_FINAL_AND_OVERRIDE
#endif

#ifdef RILL_MACRO_USE_FINAL_AND_OVERRIDE
# define RILL_CXX11_FINAL      final
# define RILL_CXX11_OVERRIDE   override
#else
# define RILL_CXX11_FINAL
# define RILL_CXX11_OVERRIDE
#endif


#endif /*RILL_CONFIG_MACROS_HPP*/
