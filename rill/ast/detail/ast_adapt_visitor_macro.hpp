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

#include "visitor_delegator.hpp"

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(node)      \
    virtual auto dispatch(                                              \
        std::shared_ptr<rill::ast::detail::raw_ast_base_type<node>> const& self, \
        detail::visitor_delegator_base const& d,                        \
        environment_base_ptr const& env,                                \
        void* storage                                                   \
        ) -> void

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(node) \
    virtual auto dispatch(                                              \
        std::shared_ptr<rill::ast::detail::raw_ast_base_type<node> const> const& self, \
        detail::visitor_delegator_base const& d,                        \
        const_environment_base_ptr const& env,                          \
        void* storage                                                   \
        ) const -> void


#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT(node_class_name) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(node_class_name) \
    { \
        RILL_DEBUG_S( std::cout << "<DISPATCH> " << #node_class_name << " ast_this: " << this->get_id() << std::endl ); \
        /* down casting */                                              \
        d.callback_from_node(                                           \
            std::static_pointer_cast<node_class_name>( self ),          \
            env,                                                        \
            storage                                                     \
            );                                                          \
    }

#define RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT(node_class_name) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(node_class_name) \
    { \
        RILL_DEBUG_S( std::cout << "<DISPATCH readonly> " << #node_class_name << " ast_this: " << this->get_id() << std::endl ); \
        /* down casting */                                              \
        d.callback_from_node(                                           \
            std::static_pointer_cast<node_class_name const>( self ),    \
            env,                                                        \
            storage                                                     \
            );                                                          \
    }

//
#define RILL_AST_ADAPT_VISITOR(class_name) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT(class_name) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT(class_name) \

#define RILL_AST_ADAPT_VISITOR_VIRTUAL(class_name) \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_UNIT_HEADER(class_name) = 0; \
    RILL_DETAIL_AST_ADAPT_VISITOR_DISPATCHER_READONLY_UNIT_HEADER(class_name) = 0; \

#endif /*RILL_AST_DETAIL_AST_ADAPT_VIDITOR_MACRO_HPP*/
