//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_SPECIFIER
#define RILL_AST_DETAIL_SPECIFIER

// Use these macros under "ast" NAMESPACE ONLY.
#define RILL_AST_SPECIFY_BASETYPE( node_name, base_name ) \
    namespace detail \
    { \
        template<> \
        struct base_type_specifier<node_name> \
        { \
            typedef base_name type; \
        }; \
    }

// TODO: support multi params
#define RILL_AST_SPECIFY_BASETYPE_TEMPLATE( node_name, base_name ) \
    namespace detail \
    { \
        template<typename P0> \
        struct base_type_specifier<node_name<P0>> \
        { \
            typedef base_name type; \
        }; \
    }

#define RILL_AST_FWD_DECL_BASE( node_name ) \
    struct node_name; \
    typedef std::shared_ptr<node_name>          node_name ## _ptr; \
    typedef std::shared_ptr<node_name const>    const_ ## node_name ## _ptr;


// TODO: support multi params
#define RILL_AST_FWD_DECL_BASE_TEMPLATE( node_name ) \
    template<typename> struct node_name; \
    /*template<typename P0> using node_name ## _ptr             = std::shared_ptr<node_name<P0>>; \
    template<typename P0> using const_ ## node_name ## _ptr   = std::shared_ptr<node_name<P0> const>;*/


#define RILL_AST_FWD_DECL( node_name, Base ) \
    RILL_AST_FWD_DECL_BASE( node_name ) \
    RILL_AST_SPECIFY_BASETYPE( node_name, Base )

// TODO: support multi params
#define RILL_AST_FWD_DECL_TEMPLATE( node_name, Base ) \
    RILL_AST_FWD_DECL_BASE_TEMPLATE( node_name ) \
    RILL_AST_SPECIFY_BASETYPE_TEMPLATE( node_name, Base )

#define RILL_AST_FWD_DECL_IN_NAMESPACE( NS, node_name, Base ) \
    namespace NS { RILL_AST_FWD_DECL_BASE( node_name ) } \
    RILL_AST_SPECIFY_BASETYPE( NS::node_name, Base )

namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename=void>
            struct base_type_specifier {};

        } // namesapce detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_SPECIFIER*/
