//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_DISPATCH_FUNCTIONS_HPP
#define RILL_AST_DETAIL_DISPATCH_FUNCTIONS_HPP

#include <memory>

#include <boost/mpl/map.hpp>
#include <boost/mpl/at.hpp>

#include "../../config/macros.hpp"
#include "../../environment_fwd.hpp"

#include "../statement_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../value_fwd.hpp"
#include "../root_fwd.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // dispatch types
            struct dispatch_as_environment_tag {};
            struct dispatch_as_value_tag {};
            struct dispatch_as_type_tag {};

            typedef boost::mpl::map<
                boost::mpl::pair<dispatch_as_environment_tag,   environment_ptr>,
                boost::mpl::pair<dispatch_as_value_tag,         ast::value_ptr>,
                boost::mpl::pair<dispatch_as_type_tag,          ast::intrinsic::identifier_value_ptr>
            > as_type;

        } // namespace detail

#if 0
        template<typename NodeT, typename VisitorT>
        auto dispatch(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            -> void
        {
            node->dispatch( detail::dispatch_as_environment_tag/*TODO: fixit*/(), node, visitor, env );
        }
#endif

        template<typename NodeT, typename VisitorT>
        auto dispatch_as_env(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            // -> std::shared_ptr<void>
            -> decltype(( node->dispatch( detail::dispatch_as_environment_tag(), node, visitor, env ) ))
        {
            return node->dispatch( detail::dispatch_as_environment_tag(), node, visitor, env );
        }

        template<typename NodeT, typename VisitorT>
        auto dispatch_as_value(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            -> decltype( node->dispatch( detail::dispatch_as_value_tag(), node, visitor, env ) )
        {
            return node->dispatch( detail::dispatch_as_value_tag(), node, visitor, env );
        }

        template<typename NodeT, typename VisitorT>
        auto dispatch_as_type(
            std::shared_ptr<NodeT> const& node,
            VisitorT const& visitor,
            environment_ptr const& env
            )
            -> decltype( node->dispatch( detail::dispatch_as_type_tag(), node, visitor, env ) )
        {
            return node->dispatch( detail::dispatch_as_type_tag(), node, visitor, env );
        }

    } // namespace ast

    using ast::dispatch_as_env;
    using ast::dispatch_as_value;
    using ast::dispatch_as_type;
} // namespace rill

#endif /*RILL_AST_DETAIL_DISPATCH_FUNCTIONS_HPP*/
