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

#define RILL_AST_FWD_DECL_BASE( Node, Base ) \
    struct Node; \
    typedef std::shared_ptr<Node>           Node ## _ptr; \
    typedef std::shared_ptr<Node const>     const_ ## Node ## _ptr;

#define RILL_AST_FWD_DECL( Node, Base ) \
    RILL_AST_FWD_DECL_BASE( Node, Base ) \
    RILL_AST_SPECIFY_BASETYPE( Node, Base )

#define RILL_AST_FWD_DECL_IN_NAMESPACE( NS, Node, Base ) \
    namespace NS { RILL_AST_FWD_DECL_BASE( Node, Base ) } \
    RILL_AST_SPECIFY_BASETYPE( NS::Node, Base )

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
