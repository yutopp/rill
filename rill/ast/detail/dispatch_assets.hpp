//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_DISPATCH_ASSETS_HPP
#define RILL_AST_DETAIL_DISPATCH_ASSETS_HPP

#include <memory>

#include "../../config/macros.hpp"
#include "../../environment_fwd.hpp"

#include "tree_visitor_base.hpp"
#include "dispatch_functions.hpp"
#include "specifier.hpp"


#define RILL_AST_ADAPT_VISITOR_DISPATCHER( class_name, tag ) \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type> const& self_pointer, \
        rill::ast::detail::tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, tag>::type> const& visitor, \
        environment_ptr const& env \
        ) const \
        -> rill::ast::detail::tree_visitor_base< \
            boost::mpl::at<rill::ast::detail::as_type, tag>::type \
        >::template result<class_name>::type /*/ \
        -> decltype( visitor( std::shared_ptr<class_name>(), env ) )/**/ RILL_CXX11_OVERRIDE \
    { \
        return visitor( std::static_pointer_cast<class_name>( self_pointer ), env ); \
    }


// insert this macro into AST node class
#define RILL_AST_ADAPT_VISITOR( class_name ) \
    RILL_AST_ADAPT_VISITOR_DISPATCHER( class_name, rill::ast::detail::dispatch_as_environment_tag ) \
    RILL_AST_ADAPT_VISITOR_DISPATCHER( class_name, rill::ast::detail::dispatch_as_value_tag ) \
    RILL_AST_ADAPT_VISITOR_DISPATCHER( class_name, rill::ast::detail::dispatch_as_type_tag )

#endif /*RILL_AST_DETAIL_DISPATCH_ASSETS_HPP*/
