//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <iostream>
#include <cstddef>

#include "config/macros.hpp"

#include "environment_fwd.hpp"

#include "ast/value_fwd.hpp"
#include "ast/expression_fwd.hpp"
#include "ast/statement_fwd.hpp"
#include "ast/root_fwd.hpp"


#define RILL_TV_OP_DECL( node_type ) \
    auto operator()( node_type const&, environment_ptr const& ) const \
        -> result<node_type>::type RILL_CXX11_OVERRIDE;

#define RILL_TV_OP( class_name, node_type, node_name, env_name ) \
    auto class_name::operator()( node_type const& node_name, environment_ptr const& env_name ) const \
        -> class_name::result<node_type>::type


///
#define RILL_TV_BASE_VOID_OP( node_type ) \
    virtual void operator()( node_type const& node, environment_ptr const& env ) const \
    { \
        this->unimplemented<node_type>(); \
    }

#define RILL_TV_BASE_RETURN_OP( node_type ) \
    virtual auto operator()( node_type const& node, environment_ptr const& env ) const \
        -> typename result<node_type>::type \
    { \
        this->unimplemented<node_type>(); \
        return nullptr; \
    }
///

namespace rill
{
    namespace detail
    {
        template<typename R, typename C>
        struct tree_visitor_result;

        template<typename R> struct tree_visitor_result<R, ast::root> { typedef void type; };

        template<typename R> struct tree_visitor_result<R, ast::statement> { typedef void type; };

        template<typename R> struct tree_visitor_result<R, ast::expression> { typedef R type; };

        template<typename R> struct tree_visitor_result<R, ast::value> { typedef R type; };
    } // namespace detail

    template<typename ReturnT>
    struct tree_visitor_base
    {
    public:
        template<typename NodeT>
        struct result
        {
            typedef typename detail::tree_visitor_result<ReturnT, typename ast::detail::base_type_specifier<NodeT>::type>::type type;
        };

    public:
        virtual ~tree_visitor_base() {}

    public:
        //
        RILL_TV_BASE_VOID_OP( ast::root )

        // statement
        // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;
        RILL_TV_BASE_VOID_OP( ast::expression_statement )
        RILL_TV_BASE_VOID_OP( ast::return_statement )
        RILL_TV_BASE_VOID_OP( ast::function_definition_statement )
        RILL_TV_BASE_VOID_OP( ast::class_definition_statement )


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
        template<typename NodeT>
        auto operator()( NodeT const&, environment_ptr const& ) const
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
                << "in " << typeid( *this ).name() << std::endl
                << " -> " << typeid( NodeT ).name() << std::endl;
        }
    };

} // namespace rill

#undef RILL_TV_BASE_VOID_OP
#undef RILL_TV_BASE_RETURN_OP
