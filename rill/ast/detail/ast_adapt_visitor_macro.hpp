//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_AST_ADAPT_VIDITOR_MACRO_HPP
#define RILL_AST_DETAIL_AST_ADAPT_VIDITOR_MACRO_HPP

#include <memory>

// import RILL_DISPATCH_TYPES_SEQ
#include "../../config/tags_for_ast_dispatching.hpp"
#include "../../config/macros.hpp"


#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(node_class_name, tag, qual) \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<node_class_name>::type> const& self_pointer, \
        rill::ast::detail::tree_visitor_base<rill::ast::detail::tag_to_type<tag>> qual& visitor, \
        environment_base_ptr const& env \
        ) \
        -> rill::ast::detail::tree_visitor_base< \
                rill::ast::detail::tag_to_type<tag> \
           >::result<node_class_name>::type

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(node_class_name, tag, qual) \
    virtual auto dispatch( \
        tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<node_class_name>::type const> const& self_pointer, \
        rill::ast::detail::tree_visitor_base<rill::ast::detail::tag_to_type<tag>> qual& visitor, \
        const_environment_base_ptr const& env \
        ) const \
        -> rill::ast::detail::tree_visitor_base< \
                rill::ast::detail::tag_to_type<tag> \
           >::result<node_class_name>::type


#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT(node_class_name, tag, qual) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(node_class_name, tag, qual) \
    { \
        RILL_DEBUG_S( std::cout << "<DISPATCH> " << #node_class_name << " ast_this: " << this->get_id() << std::endl ); \
        /* down casting */ \
        return visitor.invoke( std::static_pointer_cast<node_class_name>( self_pointer ), env ); \
    }

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT(node_class_name, tag, qual) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(node_class_name, tag, qual) \
    { \
        RILL_DEBUG_S( std::cout << "<DISPATCH> " << #node_class_name << " ast_this: " << this->get_id() << std::endl ); \
        /* down casting */ \
        return visitor.invoke( std::static_pointer_cast<node_class_name const>( self_pointer ), env ); \
    }


// class_name means node_name...
#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_(class_name, tag) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT(class_name, tag, ) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT(class_name, tag, const) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT(class_name, tag, ) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT(class_name, tag, const)

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER(r, class_name, elem) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_(class_name, rill::ast::detail:: BOOST_PP_TUPLE_ELEM(2, 0/*tag*/, elem))



#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL_(class_name, tag) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(class_name, tag, ) = 0; \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(class_name, tag, const) = 0; \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(class_name, tag,) = 0; \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(class_name, tag, const) = 0;

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL(r, class_name, elem) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL_(class_name, rill::ast::detail:: BOOST_PP_TUPLE_ELEM(2, 0/*tag*/, elem))


//
#define RILL_AST_ADAPT_VISITOR(class_name) \
    BOOST_PP_SEQ_FOR_EACH(RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER, class_name, RILL_DISPATCH_TYPES_SEQ)

#define RILL_AST_ADAPT_VISITOR_VIRTUAL(class_name) \
    BOOST_PP_SEQ_FOR_EACH(RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_VIRTUAL, class_name, RILL_DISPATCH_TYPES_SEQ)


#endif /*RILL_AST_DETAIL_AST_ADAPT_VIDITOR_MACRO_HPP*/
