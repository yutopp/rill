//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_VISITOR_HPP
#define RILL_AST_VISITOR_HPP

#include "detail/tree_visitor_base.hpp"
#include "../message/dummy_message_container.hpp"


namespace rill
{
    namespace ast
    {
        // AST Node: NOT const, visitor: NOT const
        template<typename Derived, typename ReturnT, typename Messaging = message::dummy_message_container>
        struct ast_visitor
            : public detail::tree_visitor_base<Derived, ReturnT, Messaging>
        {
        public:
            typedef ast_visitor                         self_type;
            typedef self_type const                     const_self_type;

            typedef self_type                           base_type;

        public:
            virtual ~ast_visitor() {}

        public:
            // send messages to AST node
            template<typename Node,
                     typename Env = environment_base,
                     typename = typename std::enable_if<
                         (!std::is_const<Node>::value)
                         && (!std::is_const<Env>::value)
                         >::type
                     >
            auto dispatch(
                std::shared_ptr<Node> const& node,
                std::shared_ptr<Env> const& env = nullptr
                )
                -> typename self_type::template result_type<Node>
            {
                assert( node != nullptr );
                return this->invoke( node, env );
            }

        public:
            template<typename Node, typename EnvPtr>
            auto operator()(
                std::shared_ptr<Node> const&,
                EnvPtr const&
                )
                -> typename self_type::template result_type<Node>
            {
                return this->template failed_to_dispatch<Node>();
            }
        };


        // AST Node: NOT const, visitor: const
        template<typename Derived, typename ReturnT, typename Messaging = message::dummy_message_container>
        struct ast_visitor_const
            : public detail::tree_visitor_base<Derived const, ReturnT, Messaging>
        {
        public:
            typedef ast_visitor_const                   self_type;
            typedef self_type const                     const_self_type;

            typedef self_type                           base_type;

        public:
            virtual ~ast_visitor_const() {}

        public:
            // send messages to AST node
            template<typename Node,
                     typename Env = environment_base,
                     typename = typename std::enable_if<
                         (!std::is_const<Node>::value)
                         && (!std::is_const<Env>::value)
                         >::type
                     >
            auto dispatch(
                std::shared_ptr<Node> const& node,
                std::shared_ptr<Env> const& env = nullptr
                ) const
                -> typename self_type::template result_type<Node>
            {
                assert( node != nullptr );
                return this->invoke( node, env );
            }

        public:
            template<typename Node, typename EnvPtr>
            auto operator()(
                std::shared_ptr<Node> const&,
                EnvPtr const&
                ) const
                -> typename self_type::template result_type<Node>
            {
                return this->template failed_to_dispatch<Node>();
            }
        };


        // AST Node: const, visitor: NOT const
        template<typename Derived, typename ReturnT, typename Messaging = message::dummy_message_container>
        struct readonly_ast_visitor
            : public detail::tree_visitor_base<Derived, ReturnT, Messaging>
        {
        public:
            typedef readonly_ast_visitor                self_type;
            typedef self_type const                     const_self_type;

            typedef self_type                           base_type;

        public:
            virtual ~readonly_ast_visitor() {}

        public:
            template<typename Node, typename Env = const environment_base>
            auto dispatch(
                std::shared_ptr<Node const> const& node,
                std::shared_ptr<Env const> const& env = nullptr
                )
                -> typename self_type::template result_type<Node>
            {
                assert( node != nullptr );
                return this->invoke( node, env );
            }

            template<typename Node, typename Env = const environment_base>
            auto dispatch(
                std::shared_ptr<Node> const& node,
                std::shared_ptr<Env> const& env = nullptr
                )
                -> typename self_type::template result_type<Node>
            {
                return this->dispatch(
                    std::const_pointer_cast<Node const>( node ),
                    std::const_pointer_cast<Env const>( env )
                    );
            }

        public:
            template<typename Node, typename EnvPtr>
            auto operator()(
                std::shared_ptr<Node> const&,
                EnvPtr const&
                )
                -> typename self_type::template result_type<Node>
            {
                return this->template failed_to_dispatch<Node>();
            }
        };


        // AST Node: const, visitor: const
        template<typename Derived, typename ReturnT, typename Messaging = message::dummy_message_container>
        struct readonly_ast_visitor_const
            : public detail::tree_visitor_base<Derived const, ReturnT, Messaging>
        {
        public:
            typedef readonly_ast_visitor_const          self_type;
            typedef self_type const                     const_self_type;

            typedef self_type                           base_type;

        public:
            virtual ~readonly_ast_visitor_const() {}

        public:
            template<typename Node, typename Env = const environment_base>
            auto dispatch(
                std::shared_ptr<Node const> const& node,
                std::shared_ptr<Env const> const& env = nullptr
                ) const
                -> typename self_type::template result_type<Node>
            {
                assert( node != nullptr );
                return this->invoke( node, env );
            }

            template<typename Node, typename Env = const environment_base>
            auto dispatch(
                std::shared_ptr<Node> const& node,
                std::shared_ptr<Env> const& env = nullptr
                ) const
                -> typename self_type::template result_type<Node>
            {
                return this->dispatch(
                    std::const_pointer_cast<Node const>( node ),
                    std::const_pointer_cast<Env const>( env )
                    );
            }

        public:
            template<typename Node, typename EnvPtr>
            auto operator()(
                std::shared_ptr<Node> const&,
                EnvPtr const&
                ) const
                -> typename self_type::template result_type<Node>
            {
                return this->template failed_to_dispatch<Node>();
            }
        };

    } // namespace ast
} // namespace rill


#endif /*RILL_AST_VISITOR_HPP*/
