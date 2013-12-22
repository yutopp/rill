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
#include "../../environment/environment_fwd.hpp"

#include "tree_visitor_base.hpp"
#include "dispatch_functions.hpp"
#include "specifier.hpp"


#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_(class_name, tag) \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type> const& self_pointer, \
        rill::ast::detail::tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, tag>::type>& visitor, \
        environment_base_ptr const& env \
        ) \
        -> rill::ast::detail::tree_visitor_base< \
                boost::mpl::at<rill::ast::detail::as_type, tag>::type \
           >::result<class_name>::type \
    { \
        return visitor.invoke( std::static_pointer_cast<class_name>( self_pointer ), env ); \
    } \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type const> const& const_self_pointer, \
        rill::ast::detail::tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, tag>::type> const& visitor, \
        const_environment_base_ptr const& env \
        ) const \
        -> rill::ast::detail::tree_visitor_base< \
                boost::mpl::at<rill::ast::detail::as_type, tag>::type \
           >::result<class_name>::type \
    { \
        return visitor.invoke( std::static_pointer_cast<class_name const>( const_self_pointer ), env ); \
    }

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER(r, class_name, elem) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_(class_name, rill::ast::detail:: BOOST_PP_TUPLE_ELEM(2, 0/*tag*/, elem))




#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL_(class_name, tag) \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type> const& self_pointer, \
        rill::ast::detail::tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, tag>::type>& visitor, \
        environment_base_ptr const& env \
        ) \
        -> rill::ast::detail::tree_visitor_base< \
                boost::mpl::at<rill::ast::detail::as_type, tag>::type \
           >::result<class_name>::type =0; \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type const> const& const_self_pointer, \
        rill::ast::detail::tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, tag>::type> const& visitor, \
        const_environment_base_ptr const& env \
        ) const \
        -> rill::ast::detail::tree_visitor_base< \
                boost::mpl::at<rill::ast::detail::as_type, tag>::type \
           >::result<class_name>::type =0;




#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL(r, class_name, elem) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL_(class_name, rill::ast::detail:: BOOST_PP_TUPLE_ELEM(2, 0/*tag*/, elem))


// !!! --
//
// insert this macro into AST node class
//
// -- !!!
#define RILL_AST_ADAPT_VISITOR(class_name) \
    BOOST_PP_SEQ_FOR_EACH(RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER, class_name, RILL_DISPATCH_TYPES_SEQ)

#define RILL_AST_ADAPT_VISITOR_VIRTUAL(class_name) \
    BOOST_PP_SEQ_FOR_EACH(RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL, class_name, RILL_DISPATCH_TYPES_SEQ)


//#undef RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER
//#undef RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_

#endif /*RILL_AST_DETAIL_DISPATCH_ASSETS_HPP*/
