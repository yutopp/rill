//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <memory>

#include <boost/type_erasure/any.hpp>
#include <boost/type_erasure/member.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/map.hpp>

#include "../../config/macros.hpp"

#include "../../environment_fwd.hpp"
#include "../statement_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../value_fwd.hpp"
#include "../root_fwd.hpp"

#include "specifier.hpp"


//
#define RILL_AST_ADAPT_VISITOR( class_name ) \
    virtual auto dispatch( \
        rill::ast::detail::dispatch_as_environment_tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type> self_pointer, \
        tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, rill::ast::detail::dispatch_as_environment_tag>::type> const& visitor, \
        environment_ptr const& env \
        ) const \
        -> tree_visitor_base< \
            boost::mpl::at<rill::ast::detail::as_type, rill::ast::detail::dispatch_as_environment_tag>::type \
        >::template result<class_name>::type RILL_CXX11_OVERRIDE \
    { \
        return visitor( std::dynamic_pointer_cast<class_name>( self_pointer ), env ); \
    } \
    \
    virtual auto dispatch( \
        rill::ast::detail::dispatch_as_value_tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type> self_pointer, \
        tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, rill::ast::detail::dispatch_as_value_tag>::type> const& visitor, \
        environment_ptr const& env \
        ) const \
        -> tree_visitor_base< \
            boost::mpl::at<rill::ast::detail::as_type, rill::ast::detail::dispatch_as_value_tag>::type \
        >::template result<class_name>::type RILL_CXX11_OVERRIDE \
    { \
        return visitor( std::dynamic_pointer_cast<class_name>( self_pointer ), env ); \
    } \
    \
    virtual auto dispatch( \
        rill::ast::detail::dispatch_as_type_tag, \
        std::shared_ptr<rill::ast::detail::base_type_specifier<class_name>::type> self_pointer, \
        tree_visitor_base<boost::mpl::at<rill::ast::detail::as_type, rill::ast::detail::dispatch_as_type_tag>::type> const& visitor, \
        environment_ptr const& env \
        ) const \
        -> tree_visitor_base< \
            boost::mpl::at<rill::ast::detail::as_type, rill::ast::detail::dispatch_as_type_tag>::type \
        >::template result<class_name>::type RILL_CXX11_OVERRIDE \
    { \
        return visitor( std::dynamic_pointer_cast<class_name>( self_pointer ), env ); \
    }


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            //
            struct dispatch_as_environment_tag {};
            struct dispatch_as_value_tag {};
            struct dispatch_as_type_tag {};

            typedef boost::mpl::map<
                boost::mpl::pair<dispatch_as_environment_tag,   environment_ptr>,
                boost::mpl::pair<dispatch_as_value_tag,         ast::value_ptr>,
                boost::mpl::pair<dispatch_as_type_tag,          ast::intrinsic::identifier_value_ptr>
            > as_type;
        } // namespace detail

    template<typename NodeT>
    auto dispatch_as_env(
        std::shared_ptr<NodeT> const& node,
        tree_visitor_base<boost::mpl::at<detail::as_type, detail::dispatch_as_environment_tag>::type> const& visitor,
        environment_ptr const& env
        )
        -> decltype( node->dispatch( detail::dispatch_as_environment_tag(), node, visitor, env ) )
    {
        return node->dispatch( detail::dispatch_as_environment_tag(), node, visitor, env );
    }

    template<typename NodeT>
    auto dispatch_as_value(
        std::shared_ptr<NodeT> const& node,
        tree_visitor_base<boost::mpl::at<detail::as_type, detail::dispatch_as_value_tag>::type> const& visitor,
        environment_ptr const& env
        )
        -> decltype( node->dispatch( detail::dispatch_as_value_tag(), node, visitor, env ) )
    {
        return node->dispatch( detail::dispatch_as_value_tag(), node, visitor, env );
    }

    template<typename NodeT>
    auto dispatch_as_type(
        std::shared_ptr<NodeT> const& node,
        tree_visitor_base<boost::mpl::at<detail::as_type, detail::dispatch_as_type_tag>::type> const& visitor,
        environment_ptr const& env
        )
        -> decltype( node->dispatch( detail::dispatch_as_type_tag(), node, visitor, env ) )
    {
        return node->dispatch( detail::dispatch_as_type_tag(), node, visitor, env );
    }

    } // namespace ast
}



//
BOOST_TYPE_ERASURE_MEMBER( (rill)(ast)(detail)(has_dispatch),   dispatch,   4 )
namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename NodeT>
            struct dispatcher_concept
            {
                typedef typename std::shared_ptr<typename rill::ast::detail::base_type_specifier<NodeT>::type>  self_pointer;

                typedef boost::mpl::vector<
                    has_dispatch<
                        typename tree_visitor_base<boost::mpl::at<as_type, dispatch_as_environment_tag>::type>::template result<NodeT>::type
                        ( dispatch_as_environment_tag
                        , self_pointer
                        , tree_visitor_base<boost::mpl::at<as_type, dispatch_as_environment_tag>::type>
                        , environment_ptr
                        )
                        >,
                    has_dispatch<
                        typename tree_visitor_base<boost::mpl::at<as_type, dispatch_as_value_tag>::type>::template result<NodeT>::type
                        ( dispatch_as_environment_tag
                        , self_pointer
                        , tree_visitor_base<boost::mpl::at<as_type, dispatch_as_value_tag>::type>
                        , environment_ptr
                        )
                        >,
                    has_dispatch<
                        typename tree_visitor_base<boost::mpl::at<as_type, dispatch_as_type_tag>::type>::template result<NodeT>::type
                        ( dispatch_as_environment_tag
                        , self_pointer
                        , tree_visitor_base<boost::mpl::at<as_type, dispatch_as_type_tag>::type>
                        , environment_ptr
                        )
                        >
                > type;
            };
        } // namespace detail
    } // namespace ast
} // namespace rill
