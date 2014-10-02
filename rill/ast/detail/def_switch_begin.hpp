//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include "macros_for_forward_decl.hpp"
#include "macros_for_visitor_delegator.hpp"


//// ========================================
#if defined(RILL_AST_MAKE_DEFINITION)
//// ========================================
//// in definition
//// ========================================
# define RILL_AST_DEF( class_name, group ) /* DO NOTHING */

# define RILL_AST_DEF_IN_NAMESPACE( ns, class_name, group ) /* DO NOTHING */

# define RILL_AST_DEF_GROUP( group )                                    \
    namespace detail {                                                  \
    auto clone_ast_node( const_ ## group ## _ptr const& ast )           \
        -> group ## _ptr                                                \
    {                                                                   \
        return ast != nullptr ? ast->clone() : nullptr;                 \
    }                                                                   \
    auto dump_ast_node(                                                 \
        const_ ## group ## _ptr const& ast,                             \
        std::ostream& os,                                               \
        std::string const& space                                        \
        )                                                               \
        -> void                                                         \
    {                                                                   \
        if ( ast != nullptr ) {                                         \
            ast->dump( os, space.size() + 1 );                          \
        } else {                                                        \
            os << space << "nullptr" << std::endl;                      \
        }                                                               \
    }                                                                   \
    } /* namespace detail */                                            \

# define RILL_AST_REQ_NS


//// ========================================
#elif defined(RILL_AST_ENUM_DELEGETOR_SIGNATURE)
//// ========================================
//// in enumration for delegator signature
//// ========================================
# define RILL_AST_DEF( class_name, group ) \
    RILL_VISITOR_DELEGATOR_INTERFACE( class_name )

# define RILL_AST_DEF_IN_NAMESPACE( ns, class_name, group ) \
    RILL_VISITOR_DELEGATOR_INTERFACE( ns :: class_name )

# define RILL_AST_DEF_GROUP( group ) \
    RILL_VISITOR_DELEGATOR_INTERFACE( group )



//// ========================================
#elif defined(RILL_AST_ENUM_DELEGETOR_FUNCTION)
//// ========================================
//// in enumration for delegator function
//// ========================================
# define RILL_AST_DEF( class_name, group ) \
    RILL_VISITOR_DELEGATOR_DEFINITION( class_name )

# define RILL_AST_DEF_IN_NAMESPACE( ns, class_name, group ) \
    RILL_VISITOR_DELEGATOR_DEFINITION( ns :: class_name )

# define RILL_AST_DEF_GROUP( group )            \
    RILL_VISITOR_DELEGATOR_DEFINITION( group )


//// ========================================
#else
//// ========================================
//// in header
//// ========================================

# ifndef RILL_AST_TEMPLATE_CLONE_AST
// free function
#  define RILL_AST_TEMPLATE_CLONE_AST( class_name, group, prefix )      \
    namespace detail {                                                  \
    template<typename AstPtr = prefix class_name ## _ptr>               \
    auto clone_ast_element( prefix const_ ## class_name ## _ptr const& v ) \
        -> AstPtr                                                       \
    {                                                                   \
        return std::static_pointer_cast<typename AstPtr::element_type>( \
            clone_ast_node(                                             \
                std::static_pointer_cast<group const>( v )              \
                )                                                       \
            );                                                          \
    }                                                                   \
    template<typename AstPtr = prefix class_name ## _ptr>               \
    auto clone_ast_element( prefix class_name ## _ptr const& v )        \
        -> AstPtr                                                       \
    {                                                                   \
        return std::static_pointer_cast<typename AstPtr::element_type>( \
            clone_ast_node(                                             \
                std::static_pointer_cast<group const>( v )              \
                )                                                       \
            );                                                          \
    }                                                                   \
    template<typename AstPtr = prefix class_name ## _ptr>               \
    auto dump_ast_element(                                              \
        prefix class_name ## _ptr const& v,                             \
        std::ostream& os,                                               \
        std::string const& space                                        \
        )                                                               \
        -> void                                                         \
    {                                                                   \
        dump_ast_node(                                                  \
            std::static_pointer_cast<group const>( v ), os, space       \
            );                                                          \
    }                                                                   \
    } /* namespace detail */                                            \

# endif /* ifndef RILL_AST_TEMPLATE_CLONE_AST */

# define RILL_AST_DEF( class_name, group )              \
    RILL_AST_FWD_DECL( class_name, group )              \
    RILL_AST_TEMPLATE_CLONE_AST( class_name, group, )

# define RILL_AST_DEF_IN_NAMESPACE( ns, class_name, group ) \
    RILL_AST_FWD_DECL_IN_NAMESPACE( ns, class_name, group ) \
    RILL_AST_TEMPLATE_CLONE_AST( class_name, group, ns:: )

# define RILL_AST_DEF_GROUP( group )                                    \
    RILL_AST_FWD_DECL( group, group )                                   \
    /* only declaration*/                                               \
    namespace detail {                                                  \
    auto clone_ast_node( const_ ## group ## _ptr const& )               \
        -> group ## _ptr;                                               \
    auto dump_ast_node(                                                 \
        const_ ## group ## _ptr const&,                                 \
        std::ostream&,                                                  \
        std::string const&                                              \
        ) -> void;                                                      \
    } /* namespace detail */                                            \
    RILL_AST_TEMPLATE_CLONE_AST( group, group, )

# define RILL_AST_REQ_NS

#endif
