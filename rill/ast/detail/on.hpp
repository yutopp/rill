#include "specifier.hpp"


#ifdef RILL_AST_MAKE_DEFINITION
// in definition
# define RILL_AST_DEF( class_name, group )

# define RILL_AST_DEF_IN_NAMESPACE( ns, class_name, group )


# define RILL_AST_DEF_GROUP( group ) \
    auto clone_ast_bridge( const_ ## group ## _ptr const& ast ) -> group ## _ptr \
    {                                                                   \
        return ast->clone();                                            \
    }



#else

#ifndef RILL_AST_TEMPLATE_CLONE_AST
// in header
#define RILL_AST_TEMPLATE_CLONE_AST( class_name, group, prefix )        \
    template<typename AstPtr = prefix class_name ## _ptr>               \
    auto clone_ast( prefix const_ ## class_name ## _ptr const& v )      \
        -> AstPtr                                                       \
    {                                                                   \
        return std::static_pointer_cast<typename AstPtr::element_type>( \
            clone_ast_bridge( std::static_pointer_cast<const group>( v ) ) \
            );                                                          \
    }                                                                   \
    template<typename AstPtr = prefix class_name ## _ptr>               \
    auto clone_ast( prefix class_name ## _ptr const& v )                \
        -> AstPtr                                                       \
    {                                                                   \
        return std::static_pointer_cast<typename AstPtr::element_type>( \
            clone_ast_bridge( std::static_pointer_cast<const group>( v ) ) \
            );                                                          \
    }
#endif

# define RILL_AST_DEF( class_name, group ) \
    RILL_AST_FWD_DECL( class_name, group ) \
    RILL_AST_TEMPLATE_CLONE_AST( class_name, group, )

# define RILL_AST_DEF_IN_NAMESPACE( ns, class_name, group ) \
    RILL_AST_FWD_DECL_IN_NAMESPACE( ns, class_name, group ) \
    RILL_AST_TEMPLATE_CLONE_AST( class_name, group, ns:: )


#define RILL_AST_DEF_GROUP( group )                                     \
    RILL_AST_FWD_DECL( group, group )                                   \
    auto clone_ast_bridge( const_ ## group ## _ptr const& ) -> group ## _ptr; \
    RILL_AST_TEMPLATE_CLONE_AST( group, group, )

#endif
