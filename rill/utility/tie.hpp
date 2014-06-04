//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_UTILITY_TIE_HPP
#define RILL_UTILITY_TIE_HPP

#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/variadic/to_seq.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/reverse.hpp>
#include <boost/preprocessor/seq/pop_back.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>


#define RILL_PP_TIE_TUPLE_DECL( expr )                              \
    auto&& BOOST_PP_CAT( _rill_pp_tuple_all_,  __LINE__ ) = expr;

#define RILL_PP_TIE_DECL( r, data, i, elem )                            \
    auto&& elem = std::get<i>( BOOST_PP_CAT( _rill_pp_tuple_all_,  __LINE__ ) );

#define RILL_PP_TIE( ... )                              \
    RILL_PP_TIE_TUPLE_DECL(                             \
        BOOST_PP_SEQ_HEAD(                              \
            BOOST_PP_SEQ_REVERSE(                       \
                BOOST_PP_VARIADIC_TO_SEQ( __VA_ARGS__ ) \
                )                                       \
            )                                           \
        )                                               \
                                                        \
    BOOST_PP_SEQ_FOR_EACH_I(                            \
        RILL_PP_TIE_DECL,                               \
        _,                                              \
        BOOST_PP_SEQ_POP_BACK(                          \
            BOOST_PP_VARIADIC_TO_SEQ( __VA_ARGS__ )     \
            )                                           \
        )

// Usage:
// RILL_PP_TIE( a, b, ..., tupled_value )

#endif /*RILL_UTILITY_TIE_HPP*/
