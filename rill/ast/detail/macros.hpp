//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "../../config/macros.hpp"

#include "specifier.hpp"


//
#define RILL_AST_ADAPT_VISITOR( class_name ) \
    virtual auto dispatch_as_env( tree_visitor_base<environment_ptr> const& visitor, environment_ptr const& env ) const \
        -> tree_visitor_base< \
            environment_ptr \
        >::template result<class_name>::type RILL_CXX11_OVERRIDE \
    { \
        return visitor( *this, env ); \
    } \
    virtual auto dispatch_as_value( tree_visitor_base<ast::value_ptr> const& visitor, environment_ptr const& env ) const \
        -> tree_visitor_base< \
            ast::value_ptr \
        >::template result<class_name>::type RILL_CXX11_OVERRIDE \
    { \
        return visitor( *this, env ); \
    } \
    virtual auto dispatch_as_type( tree_visitor_base<ast::intrinsic::identifier_value_ptr> const& visitor, environment_ptr const& env ) const \
        -> tree_visitor_base< \
            ast::intrinsic::identifier_value_ptr \
        >::template result<class_name>::type RILL_CXX11_OVERRIDE \
    { \
        return visitor( *this, env ); \
    }