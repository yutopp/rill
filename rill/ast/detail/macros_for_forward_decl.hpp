//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_MACROS_FOR_FORWARD_DECL_HPP
#define RILL_AST_DETAIL_MACROS_FOR_FORWARD_DECL_HPP

// Use these macros under "ast" NAMESPACE ONLY.
#define RILL_AST_SPECIFY_BASETYPE( node_name, base_name ) \
    namespace detail                                      \
    {                                                     \
        template<>                                        \
        struct ast_base_type_specifier<node_name>         \
        {                                                 \
            typedef base_name type;                       \
        };                                                \
    }

#define RILL_AST_FWD_DECL_BASE( node_name )                             \
    class node_name;                                                    \
    typedef std::shared_ptr<node_name>          node_name ## _ptr;      \
    typedef std::shared_ptr<node_name const>    const_ ## node_name ## _ptr;

// ========================================
//
// ========================================
//
#define RILL_AST_FWD_DECL( node_name, Base )        \
    RILL_AST_FWD_DECL_BASE( node_name )             \
    RILL_AST_SPECIFY_BASETYPE( node_name, Base )

//
#define RILL_AST_FWD_DECL_IN_NAMESPACE( NS, node_name, Base )   \
    namespace NS { RILL_AST_FWD_DECL_BASE( node_name ) }        \
    RILL_AST_SPECIFY_BASETYPE( NS::node_name, Base )
// ========================================

#endif /*RILL_AST_DETAIL_MACROS_FOR_FORWARD_DECL_HPP*/
