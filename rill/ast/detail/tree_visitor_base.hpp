//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP
#define RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP

#include <memory>
#include <iostream>
#include <cassert>  // for assert

#include "../../config/macros.hpp"

#include "../../environment_fwd.hpp"

#include "../value_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../statement_fwd.hpp"
#include "../root_fwd.hpp"

#include "dispatch_functions.hpp"


#define RILL_TV_OP_DECL( node_type ) \
    auto operator()( std::shared_ptr<node_type> const&, environment_ptr const& ) \
        -> result<node_type>::type RILL_CXX11_OVERRIDE;

#define RILL_TV_OP_DECL_CONST( node_type ) \
    auto operator()( std::shared_ptr<node_type const> const&, const_environment_ptr const& ) const \
        -> result<node_type>::type RILL_CXX11_OVERRIDE;

#define RILL_TV_OP( class_name, node_type, node_name, env_name ) \
    auto class_name::operator()( std::shared_ptr<node_type> const& node_name, environment_ptr const& env_name ) \
        -> result<node_type>::type

#define RILL_TV_OP_CONST( class_name, node_type, node_name, env_name ) \
    auto class_name::operator()( std::shared_ptr<node_type const> const& node_name, const_environment_ptr const& env_name ) const \
        -> result<node_type>::type

///
#define RILL_TV_BASE_VOID_OP( node_type ) \
    virtual void operator()( std::shared_ptr<node_type> const&, environment_ptr const& ) \
    { \
        this->unimplemented<node_type>(); \
    } \
    virtual void operator()( std::shared_ptr<node_type const> const&, const_environment_ptr const& ) const \
    { \
        this->unimplemented<node_type const>(); \
    }

///
#define RILL_TV_BASE_VOID_OP_NOTHING( node_type ) \
    virtual void operator()( std::shared_ptr<node_type> const&, environment_ptr const& ) \
    {} \
    virtual void operator()( std::shared_ptr<node_type const> const&, const_environment_ptr const& ) const \
    {}

#define RILL_TV_BASE_RETURN_OP( node_type ) \
    virtual auto operator()( std::shared_ptr<node_type> const&, environment_ptr const& ) \
        -> typename result<node_type>::type \
    { \
        this->unimplemented<node_type>(); \
        return typename result<node_type>::type(); \
    } \
    virtual auto operator()( std::shared_ptr<node_type const> const& node, const_environment_ptr const& env ) const \
        -> typename result<node_type>::type \
    { \
        this->unimplemented<node_type const>(); \
        return typename result<node_type>::type(); \
    }


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename R, typename BaseNode>
            struct tree_visitor_result;

            template<typename R> struct tree_visitor_result<R, ast::root>       { typedef void type; };
            template<typename R> struct tree_visitor_result<R, ast::statement>  { typedef void type; };
            template<typename R> struct tree_visitor_result<R, ast::expression> { typedef R type; };
            template<typename R> struct tree_visitor_result<R, ast::value>      { typedef R type; };


            // --
            // base class of ast visitor
            // --
            template<typename ReturnT>
            struct tree_visitor_base
            {
            public:
                typedef tree_visitor_base           self_type;
                typedef tree_visitor_base const     const_self_type;

                template<typename NodeT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                virtual ~tree_visitor_base() {}

            public:
                //
                template<typename Node, typename Enveronment>
                auto dispatch( std::shared_ptr<Node> const& node, std::shared_ptr<Enveronment> const& env )
                    -> decltype( dispatch_as<ReturnT>( node, *reinterpret_cast<self_type*>(0), env ) )
                {
                    return dispatch_as<ReturnT>( node, *this, env );
                }

                template<typename NodePtr>
                auto dispatch( NodePtr&& node )
                    -> decltype( dispatch( std::forward<NodePtr>( node ), environment_ptr() ) )
                {
                    return dispatch( std::forward<NodePtr>( node ), environment_ptr() );
                }


                //
                template<typename Node, typename Enveronment>
                auto dispatch( std::shared_ptr<Node> const& node, std::shared_ptr<Enveronment> const& env ) const
                    -> decltype( dispatch_as<ReturnT>( std::const_pointer_cast<Node const>( node ), *reinterpret_cast<const_self_type*>(0), std::static_pointer_cast<environment const>( env ) ) )
                {
                    auto const& p = std::const_pointer_cast<Node const>( node );
                    assert( p != nullptr );

                    return dispatch_as<ReturnT>( std::const_pointer_cast<Node const>( node ), *this, std::static_pointer_cast<environment const>( env ) );
                }

                template<typename NodePtr>
                auto dispatch( NodePtr&& node ) const
                    -> decltype( dispatch( std::forward<NodePtr>( node ), const_environment_ptr() ) )
                {
                    return dispatch( std::forward<NodePtr>( node ), const_environment_ptr() );
                }


            public:
                //
                RILL_TV_BASE_VOID_OP( ast::root )

                // statement
                // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;
                RILL_TV_BASE_VOID_OP( ast::expression_statement )
                RILL_TV_BASE_VOID_OP( ast::return_statement )
                RILL_TV_BASE_VOID_OP( ast::function_definition_statement )
                RILL_TV_BASE_VOID_OP( ast::class_definition_statement )
                RILL_TV_BASE_VOID_OP( ast::embedded_function_definition_statement )
                RILL_TV_BASE_VOID_OP( ast::extern_function_declaration_statement )
                RILL_TV_BASE_VOID_OP_NOTHING( ast::empty_statement ) // DEFAULT: skipped

                // expression
                RILL_TV_BASE_RETURN_OP( ast::binary_operator_expression )
                RILL_TV_BASE_RETURN_OP( ast::call_expression )
                RILL_TV_BASE_RETURN_OP( ast::embedded_function_call_expression )
                RILL_TV_BASE_RETURN_OP( ast::term_expression )
                RILL_TV_BASE_RETURN_OP( ast::type_identifier_expression )
                RILL_TV_BASE_RETURN_OP( ast::compiletime_return_type_expression )

                // value
                RILL_TV_BASE_RETURN_OP( ast::intrinsic_value )
                RILL_TV_BASE_RETURN_OP( ast::variable_value )

            public:
                // filter outbound object
                template<typename NodeT, typename EnveronmentPtr>
                auto operator()( std::shared_ptr<NodeT> const&, EnveronmentPtr const& ) const
                    -> typename result<NodeT>::type
                {
                    unimplemented<NodeT>();
                    return typename result<NodeT>::type();
                }

            private:
                template<typename NodeT>
                auto unimplemented() const
                    -> void
                {
                    std::cerr
                        << "!!! DEBUG: message. please implement it!" << std::endl
                        << " in " << typeid( *this ).name() << std::endl
                        << "  -> " << typeid( NodeT ).name() << std::endl;
                }
            };
        } // namespace detail
    } // namespace ast
} // namespace rill

#undef RILL_TV_BASE_VOID_OP
#undef RILL_TV_BASE_RETURN_OP


#endif /*RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP*/
