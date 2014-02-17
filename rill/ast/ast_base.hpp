//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_AST_BASE_HPP
#define RILL_AST_AST_BASE_HPP

#include <cstddef>
#include <memory>

#include <boost/preprocessor.hpp>

#include "detail/dispatch_assets.hpp"


#define RILL_AST_VAR_DECL( r, unused, elem ) \
    BOOST_PP_TUPLE_ELEM( 0, elem ) BOOST_PP_TUPLE_ELEM( 1, elem );


#define RILL_AST_DEF_CLONE_STMT_CUSTOMIZED_STATEMENT( class_name, elem ) \
    BOOST_PP_TUPLE_ELEM( 2, elem )

#define RILL_AST_DEF_CLONE_STMT_ORIGINAL( class_name, elem )            \
    cloned->BOOST_PP_TUPLE_ELEM( 1, elem )                              \
    = clone_ast<BOOST_PP_TUPLE_ELEM( 0, elem )>(                        \
        BOOST_PP_TUPLE_ELEM( 1, elem )                                  \
        );


#define RILL_AST_DEF_CLONE_EACH( r, class_name, elem )    \
    BOOST_PP_IIF(                                         \
        BOOST_PP_EQUAL( BOOST_PP_TUPLE_SIZE( elem ), 3 ), \
        RILL_AST_DEF_CLONE_STMT_CUSTOMIZED_STATEMENT,     \
        RILL_AST_DEF_CLONE_STMT_ORIGINAL                  \
        )( class_name, elem )

#define RILL_AST_DEF_CLONE( class_name, elem )                          \
    auto clone() const -> cloned_pointer_type                           \
    {                                                                   \
        auto cloned = std::make_shared<class_name>();                   \
        BOOST_PP_SEQ_FOR_EACH( RILL_AST_DEF_CLONE_EACH, class_name, elem ) \
        return cloned;                                                  \
    }

#define RILL_AST_DEF_CLONE_SINGLE( class_name   )     \
    auto clone() const -> cloned_pointer_type         \
    {                                                 \
        auto cloned = std::make_shared<class_name>(); \
        return cloned;                                \
    }


#define RILL_AST_DEF_CLONE_INTERFACE( base_class, elem )                \
    template<typename ClonedPtr>                                        \
    auto clone_value( ClonedPtr const& cloned ) const -> void           \
    {                                                                   \
        BOOST_PP_SEQ_FOR_EACH( RILL_AST_DEF_CLONE_EACH, class_name, elem ) \
    }

#define RILL_AST_DEF_CLONE_INTERFACE_SINGLE( class_name )          \
    template<typename ClonedPtr>                              \
    auto clone_value( ClonedPtr const& cloned ) const -> void \
    {                                                         \
    }



#define RILL_AST_DEF_CLONE_DERIVED( class_name, base_class, elem )      \
    auto clone() const -> cloned_pointer_type                           \
    {                                                                   \
        auto cloned = std::make_shared<class_name>();                   \
        BOOST_PP_SEQ_FOR_EACH( RILL_AST_DEF_CLONE_EACH, class_name, elem ) \
        base_class::clone_value( cloned );                              \
        return cloned;                                                  \
    }

#define RILL_AST_DEF_CLONE_DERIVED_SINGLE( class_name, base_class ) \
    auto clone() const -> cloned_pointer_type         \
    {                                                 \
        auto cloned = std::make_shared<class_name>(); \
        base_class::clone_value( cloned );            \
        return cloned;                                \
    }


#define RILL_AST_DEF_CLONE_DERIVED_INTERFACE( class_name, base_class, elem ) \
    template<typename ClonedPtr>                                        \
    auto clone_value( ClonedPtr const& cloned ) const -> void           \
    {                                                                   \
        BOOST_PP_SEQ_FOR_EACH( RILL_AST_DEF_CLONE_EACH, class_name, elem ) \
        base_class::clone_value( cloned );                              \
    }                                                                   \
    auto clone() const -> cloned_pointer_type                       \
    {                                                               \
        return nullptr;                                             \
    }

#define RILL_AST_DEF_CLONE_DERIVED_INTERFACE_SINGLE( class_name, base_class ) \
    template<typename ClonedPtr>                                        \
    auto clone_value( ClonedPtr const& cloned ) const -> void           \
    {                                                                   \
        base_class::clone_value( cloned );                              \
    }                                                                   \
    auto clone() const -> cloned_pointer_type                           \
    {                                                                   \
        assert( false );                                                \
        return nullptr;                                                 \
    }




#define RILL_AST_DEF_CLONE_VIRTUAL()                      \
    virtual auto clone() const -> cloned_pointer_type =0; \


#define RILL_AST_MEMBER_VARIABLE( class_name, decl_c )          \
    public/*private*/:                                          \
    BOOST_PP_SEQ_FOR_EACH( RILL_AST_VAR_DECL, _unused, decl_c ) \
    private:


#define RILL_MAKE_AST_INIT( class_name ) \
    public:                              \
    class_name() = default;              \

#define RILL_MAKE_AST_CORE( class_name ) \
    RILL_MAKE_AST_INIT( class_name )     \
    public:                              \
    RILL_AST_ADAPT_VISITOR( class_name ) \



#define RILL_MAKE_AST_SINGLE( class_name )          \
    RILL_MAKE_AST_CORE( class_name )                \
    public:                                         \
    RILL_AST_DEF_CLONE_SINGLE( class_name )         \


#define RILL_MAKE_AST_NORMAL( class_name, decl_c )  \
    RILL_MAKE_AST_CORE( class_name )                \
    RILL_AST_MEMBER_VARIABLE( class_name, decl_c )  \
    public:                                         \
    RILL_AST_DEF_CLONE( class_name, decl_c )        \



#define RILL_MAKE_AST_SINGLE_INTERFACE( class_name )  \
    RILL_MAKE_AST_INIT( class_name )                  \
    public:                                           \
    RILL_AST_DEF_CLONE_INTERFACE_SINGLE( class_name ) \


#define RILL_MAKE_AST_NORMAL_INTERFACE( class_name, decl_c ) \
    RILL_MAKE_AST_INIT( class_name )                         \
    RILL_AST_MEMBER_VARIABLE( class_name, decl_c )           \
    public:                                                  \
    RILL_AST_DEF_CLONE_INTERFACE( class_name, decl_c )       \



#define RILL_MAKE_AST_SINGLE_DERIVED( class_name, base_class )  \
    RILL_MAKE_AST_CORE( class_name )                            \
    public:                                                     \
    RILL_AST_DEF_CLONE_DERIVED_SINGLE( class_name, base_class ) \


#define RILL_MAKE_AST_NORMAL_DERIVED( class_name, base_class, decl_c ) \
    RILL_MAKE_AST_CORE( class_name )                                   \
    RILL_AST_MEMBER_VARIABLE( class_name, decl_c )                     \
    public:                                                            \
    RILL_AST_DEF_CLONE_DERIVED( class_name, base_class, decl_c )       \


#define RILL_MAKE_AST_SINGLE_DERIVED_INTERFACE( class_name, base_class ) \
    RILL_MAKE_AST_INIT( class_name )                                    \
    public:                                                             \
    RILL_AST_DEF_CLONE_DERIVED_INTERFACE_SINGLE( class_name, base_class ) \


#define RILL_MAKE_AST_NORMAL_DERIVED_INTERFACE( class_name, base_class, decl_c ) \
    RILL_MAKE_AST_INIT( class_name )                                    \
    RILL_AST_MEMBER_VARIABLE( class_name, decl_c )                      \
    public:                                                             \
    RILL_AST_DEF_CLONE_DERIVED_INTERFACE( class_name, base_class, decl_c ) \






// !!! --
//
// insert this macro into AST node class
//
// -- !!!
#define RILL_MAKE_AST( ... )                                        \
    BOOST_PP_IIF(                                                   \
        BOOST_PP_EQUAL( BOOST_PP_VARIADIC_SIZE( __VA_ARGS__ ), 1 ), \
        RILL_MAKE_AST_SINGLE,                                       \
        RILL_MAKE_AST_NORMAL                                        \
        )( __VA_ARGS__ )


#define RILL_MAKE_AST_INTERFACE( ... )                              \
    BOOST_PP_IIF(                                                   \
        BOOST_PP_EQUAL( BOOST_PP_VARIADIC_SIZE( __VA_ARGS__ ), 1 ), \
        RILL_MAKE_AST_SINGLE_INTERFACE,                             \
        RILL_MAKE_AST_NORMAL_INTERFACE                              \
        )( __VA_ARGS__ )

#define RILL_MAKE_AST_DERIVED( ... )                                \
    BOOST_PP_IIF(                                                   \
        BOOST_PP_EQUAL( BOOST_PP_VARIADIC_SIZE( __VA_ARGS__ ), 2 ), \
        RILL_MAKE_AST_SINGLE_DERIVED,                               \
        RILL_MAKE_AST_NORMAL_DERIVED                                \
        )( __VA_ARGS__ )

#define RILL_MAKE_AST_DERIVED_INTERFACE( ... )                      \
    BOOST_PP_IIF(                                                   \
        BOOST_PP_EQUAL( BOOST_PP_VARIADIC_SIZE( __VA_ARGS__ ), 2 ), \
        RILL_MAKE_AST_SINGLE_DERIVED_INTERFACE,                     \
        RILL_MAKE_AST_NORMAL_DERIVED_INTERFACE                      \
        )( __VA_ARGS__ )


// TODO: rename
#define RILL_MAKE_AST_BASE( class_name )            \
    public:                                         \
    RILL_AST_ADAPT_VISITOR_VIRTUAL( class_name )    \
    typedef class_name ## _ptr cloned_pointer_type; \
    RILL_AST_DEF_CLONE_VIRTUAL()





#define RILL_AST_CORE_BEGIN( class_name )       \
    struct class_name                           \
        : public ast_base                       \
    {                                           \
    public:                                     \
        virtual ~class_name() {}                \
                                                \
        RILL_MAKE_AST_BASE( class_name )        \

#define RILL_AST_CORE_END \
    };



#define RILL_AST_INTERFACE_BEGIN( class_name, group )  \
    struct class_name                           \
        : public group                          \
    {                                           \
    public:                                     \
        virtual ~class_name() {}                \
        RILL_MAKE_AST_INTERFACE(                \
            class_name                          \
            )


#define RILL_AST_INTERFACE_END                  \
    };






















namespace rill
{
    namespace ast
    {
        class ast_base
        {
        public:
            std::size_t line, column, length;
        };

        typedef std::shared_ptr<ast_base> ast_base_ptr;
        typedef std::shared_ptr<ast_base const> const_ast_base_ptr;

    } // namespace ast
} // namespace rill

#endif /*RILL_AST_AST_BASE_HPP*/
