//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_AST_DEF_MACRO_HPP
#define RILL_AST_DETAIL_AST_DEF_MACRO_HPP

#include <cstddef>
#include <memory>

#include <boost/preprocessor.hpp>

#include "ast_adapt_visitor_macro.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename T>
            auto make_ast_instance()
                -> std::enable_if_t<! std::is_abstract<T>::value, std::shared_ptr<T>>
            {
                return std::make_shared<T>();
            }

            template<typename T>
            auto make_ast_instance()
                -> std::enable_if_t<std::is_abstract<T>::value, std::shared_ptr<T>>
            {
                assert( false && "aaaaaa" );
                return nullptr;
            }

            template<typename T>
            class has_ast_base_type_t
            {
            private:
                template<typename U> static auto f( typename U::ast_base_type* ) -> std::true_type;
                template<typename U> static auto f(...) -> std::false_type;
            public:
                static constexpr auto check() -> bool
                {
                    return decltype( f<T>( nullptr ) )::value;
                }
            };
            template<typename T>
            constexpr auto has_ast_base_type() -> bool { return has_ast_base_type_t<T>::check(); }

            template<typename T>
            auto clone_ast_element( T const& )
                -> T
            {
                return T();
            }

            template<typename T>
            auto dump_ast_element(
                T const& v,
                std::ostream& os,
                std::string const& space
                )
                -> void
            {
                os << space << "..." << std::endl;
            }

            template<typename T, typename Alloc>
            auto dump_ast_element(
                std::vector<T, Alloc> const& vx,
                std::ostream& os,
                std::string const& space
                )
                -> void
            {
                os << space << "[" << std::endl;
                for( auto&& v : vx ) {
                    dump_ast_element( v, os, space + "  " );
                }
                os << space << "]" << std::endl;
            }

        } // namespace detail
    } // namespace ast
} // namespace rill


// ========================================
//
// ========================================
// elem( 0: type, 1: variable name )
// clone_ast_element is defined in def_switch_begin.hpp
#define RILL_AST_CLONE_EACH( _unused, class_name, elem )                \
    cloned->BOOST_PP_TUPLE_ELEM( 1, elem )                              \
        = detail::clone_ast_element<BOOST_PP_TUPLE_ELEM( 0, elem )>(    \
            BOOST_PP_TUPLE_ELEM( 1, elem )                              \
            );

//
#define RILL_AST_DEFINE_CLONE_ELEMENTS_FUNCITON( class_name, elem )     \
    template<typename ClonedPtr>                                        \
    auto clone_elements_to( ClonedPtr const& cloned ) const -> void     \
    {                                                                   \
        this->try_to_call_base_clone_elements_to( cloned );             \
        BOOST_PP_SEQ_FOR_EACH( RILL_AST_CLONE_EACH, class_name, elem )  \
    }                                                                   \
                                                                        \
    template<typename ClonedPtr>                                        \
    auto try_to_call_base_clone_elements_to( ClonedPtr const& cloned ) const \
        -> std::enable_if_t<detail::has_ast_base_type<typename ClonedPtr::element_type>()> \
    {                                                                   \
        ClonedPtr::element_type::ast_base_type::clone_elements_to( cloned ); \
    }                                                                   \
                                                                        \
    template<typename ClonedPtr>                                        \
    auto try_to_call_base_clone_elements_to( ClonedPtr const& cloned ) const \
        -> std::enable_if_t<! detail::has_ast_base_type<typename ClonedPtr::element_type>()> \
    {                                                                   \
    }


//
#define RILL_AST_DEFINE_CLONE_FUNCITON( class_name, elem )              \
    auto clone() const -> cloned_pointer_type                           \
    {                                                                   \
        auto cloned = detail::make_ast_instance<class_name>();          \
        clone_elements_to( cloned );                                    \
        return cloned;                                                  \
    }                                                                   \
    RILL_AST_DEFINE_CLONE_ELEMENTS_FUNCITON( class_name, elem )

//
#define RILL_AST_DEFINE_CLONE_SIGNATURE( class_name, elem )     \
    virtual auto clone() const -> cloned_pointer_type =0;       \
    RILL_AST_DEFINE_CLONE_ELEMENTS_FUNCITON( class_name, elem )
// ========================================



// ========================================
//
// ========================================
// elem( 0: type, 1: variable name )
// clone_ast_element is defined in def_switch_begin.hpp
#define RILL_AST_DUMP_EACH_( class_name, elem_name )                    \
    os << space << BOOST_PP_STRINGIZE( elem_name ) << " :" << std::endl; \
    detail::dump_ast_element( elem_name, os, space );                   \

#define RILL_AST_DUMP_EACH( _unused, class_name, elem )      \
    RILL_AST_DUMP_EACH_(                                     \
        class_name,                                          \
        BOOST_PP_TUPLE_ELEM( 1, elem )                       \
        )

//
#define RILL_AST_DEFINE_DUMP_ELEMENTS_FUNCITON( class_name, elem )      \
    auto dump_elements( std::ostream& os, std::size_t const& i ) const  \
        -> std::size_t                                                  \
    {                                                                   \
        auto const ni =                                                 \
            this->try_to_call_base_dump_elements<decltype(*this)>( os, i ); \
        auto const space = std::string( ni, ' ' );                      \
        os << space << "== " << #class_name << std::endl;         \
        BOOST_PP_SEQ_FOR_EACH( RILL_AST_DUMP_EACH, class_name, elem )   \
        return ni + 2;                                                  \
    }                                                                   \
                                                                        \
    template<typename Ast>                                              \
    auto try_to_call_base_dump_elements( std::ostream& os, std::size_t const& i ) const \
        -> std::enable_if_t<detail::has_ast_base_type<Ast>(), std::size_t> \
    {                                                                   \
        return Ast::ast_base_type::dump_elements( os, i );              \
    }                                                                   \
                                                                        \
    template<typename Ast>                                              \
    auto try_to_call_base_dump_elements( std::ostream&, std::size_t const& i ) const \
        -> std::enable_if_t<! detail::has_ast_base_type<Ast>(), std::size_t> \
    {                                                                   \
        return i;                                                       \
    }                                                                   \


//
#define RILL_AST_DEFINE_DUMP_FUNCITON( class_name, elem )               \
    auto dump( std::ostream& os, std::size_t const& i ) const -> void   \
    {                                                                   \
        dump_elements( os, i );                                         \
    }                                                                   \
    auto dump( std::ostream& os ) const -> void                         \
    {                                                                   \
        dump( os, 0 );                                                  \
    }                                                                   \
    RILL_AST_DEFINE_DUMP_ELEMENTS_FUNCITON( class_name, elem )

//
#define RILL_AST_DEFINE_DUMP_SIGNATURE( class_name, elem )              \
    virtual auto dump( std::ostream&, std::size_t const& ) const -> void =0; \
    virtual auto dump( std::ostream& ) const -> void =0;                \
    RILL_AST_DEFINE_DUMP_ELEMENTS_FUNCITON( class_name, elem )
// ========================================



// ========================================
//
// ========================================
// elem(0: type, 1: var name)
#define RILL_AST_DECL_VAR( r, unused, elem ) \
    BOOST_PP_TUPLE_ELEM( 0, elem ) BOOST_PP_TUPLE_ELEM( 1, elem );

//
#define RILL_AST_DECL_MEMBER_VARIABLES( class_name, decl_c )    \
    public/*private*/:                                          \
    BOOST_PP_SEQ_FOR_EACH( RILL_AST_DECL_VAR, _unused, decl_c ) \
    private:
// ========================================



// ========================================
//
// ========================================
#define RILL_AST_CTOR( class_name )                         \
    public:                                                 \
    class_name() = default;                                 \
    class_name( class_name const& ) = delete;               \

// ========================================



// ========================================
//
// ========================================
#define RILL_SETUP_GROUP_AST( class_name, decl_c )          \
    RILL_AST_CTOR( class_name )                             \
    RILL_AST_DECL_MEMBER_VARIABLES( class_name, decl_c )    \
    public:                                                 \
    using cloned_pointer_type = class_name ## _ptr;         \
    RILL_AST_ADAPT_VISITOR_VIRTUAL( class_name )            \
    RILL_AST_DEFINE_CLONE_SIGNATURE( class_name, decl_c )   \
    RILL_AST_DEFINE_DUMP_SIGNATURE( class_name, decl_c )    \


#define RILL_SETUP_AST( class_name, decl_c )                \
    RILL_AST_CTOR( class_name )                             \
    RILL_AST_DECL_MEMBER_VARIABLES( class_name, decl_c )    \
    public:                                                 \
    RILL_AST_ADAPT_VISITOR( class_name )                    \
    RILL_AST_DEFINE_CLONE_FUNCITON( class_name, decl_c )    \
    RILL_AST_DEFINE_DUMP_FUNCITON( class_name, decl_c )     \
// ========================================



// ========================================
//
// ========================================
#define RILL_AST_GROUP_BEGIN( ... )                                     \
    BOOST_PP_OVERLOAD( RILL_AST_GROUP_BEGIN_, __VA_ARGS__ )( __VA_ARGS__ )

#define RILL_AST_GROUP_BEGIN_1( class_name )                \
    RILL_AST_GROUP_BEGIN_2( class_name, BOOST_PP_SEQ_NIL )

#define RILL_AST_GROUP_BEGIN_2( class_name, elems )     \
    class class_name                                    \
        : public ast_base                               \
    {                                                   \
    public:                                             \
        virtual ~class_name() {}                        \
        RILL_SETUP_GROUP_AST( class_name, elems )       \


#define RILL_AST_GROUP_END                              \
    };



// ========================================
//
// ========================================
#define RILL_AST_BEGIN( ... )                                           \
    BOOST_PP_OVERLOAD( RILL_AST_BEGIN_, __VA_ARGS__ )( __VA_ARGS__ )

#define RILL_AST_BEGIN_2( class_name, base_class )  \
    RILL_AST_BEGIN_3( class_name, base_class, BOOST_PP_SEQ_NIL )

#define RILL_AST_BEGIN_3( class_name, base_class, elems )   \
    class class_name                                        \
        : public base_class                                 \
    {                                                       \
    public:                                                 \
        using ast_base_type = base_class;                   \
    public:                                                 \
        virtual ~class_name() {}                            \
        RILL_SETUP_AST( class_name, elems )                 \


#define RILL_AST_END                            \
    };


#endif /*RILL_AST_DETAIL_AST_DEF_MACRO_HPP*/
