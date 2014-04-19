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


namespace rill
{
    namespace ast
    {
        //
        template<typename Derived, typename ReturnT>
        struct ast_visitor
            : public detail::tree_visitor_base<ReturnT>
        {
        public:
            typedef ast_visitor                         self_type;
            typedef self_type const                     const_self_type;

            typedef detail::tree_visitor_base<ReturnT>  base_type;

        public:
            ast_visitor()
                : base_type( std::ref( master_invoker_ ) )
            {}

            virtual ~ast_visitor() {}

        public:
            // send messages to AST node
            template<typename Node, typename = typename std::enable_if<(!std::is_const<Node>::value)>::type>
            auto dispatch( std::shared_ptr<Node> const& node, environment_base_ptr const& env = nullptr )
                -> typename self_type::template result<Node>::type
            {
                assert( node != nullptr );
                return dispatch_as<ReturnT>( node, *this, env );
            }

        private:
            detail::visitor_invoker<Derived, ReturnT> master_invoker_;
        };


        //
        template<typename Derived, typename ReturnT>
        struct ast_visitor_const
            : public detail::tree_visitor_base<ReturnT>
        {
        public:
            typedef ast_visitor_const                   self_type;
            typedef self_type const                     const_self_type;

            typedef detail::tree_visitor_base<ReturnT>  base_type;

        public:
            ast_visitor_const()
                : base_type( std::ref( master_invoker_ ) )
            {}

            virtual ~ast_visitor_const() {}

        public:
            // send messages to AST node
            template<typename Node, typename = typename std::enable_if<(!std::is_const<Node>::value)>::type>
            auto dispatch( std::shared_ptr<Node> const& node, environment_base_ptr const& env = nullptr ) const
                -> typename self_type::template result<Node>::type
            {
                assert( node != nullptr );
                return dispatch_as<ReturnT>( node, *this, env );
            }

        private:
            detail::visitor_invoker<Derived, ReturnT> master_invoker_;
        };


        //
        template<typename Derived, typename ReturnT>
        struct readonly_ast_visitor
            : public detail::tree_visitor_base<ReturnT>
        {
        public:
            typedef readonly_ast_visitor                self_type;
            typedef self_type const                     const_self_type;

            typedef detail::tree_visitor_base<ReturnT>  base_type;

        public:
            readonly_ast_visitor()
                : base_type( std::ref( master_invoker_ ) )
            {}

            virtual ~readonly_ast_visitor() {}

        public:
            // send messages to AST node
            template<typename Node>
            auto dispatch( std::shared_ptr<Node const> const& node, const_environment_base_ptr const& env = nullptr )
                -> typename self_type::template result<Node>::type
            {
                assert( node != nullptr );
                return dispatch_as<ReturnT>( node, *this, env );
            }

            template<typename Node>
            auto dispatch( std::shared_ptr<Node> const& node, const_environment_base_ptr const& env = nullptr )
                -> typename self_type::template result<Node>::type
            {
                return dispatch( std::const_pointer_cast<Node const>( node ), env );
            }

        private:
            detail::visitor_invoker<Derived, ReturnT> master_invoker_;
        };


        //
        template<typename Derived, typename ReturnT>
        struct readonly_ast_visitor_const
            : public detail::tree_visitor_base<ReturnT>
        {
        public:
            typedef readonly_ast_visitor_const          self_type;
            typedef self_type const                     const_self_type;

            typedef detail::tree_visitor_base<ReturnT>  base_type;

        public:
            readonly_ast_visitor_const()
                : base_type( std::ref( master_invoker_ ) )
            {}

            virtual ~readonly_ast_visitor_const() {}

        public:
            // send messages to AST node
            template<typename Node>
            auto dispatch( std::shared_ptr<Node const> const& node, const_environment_base_ptr const& env = nullptr ) const
                -> typename self_type::template result<Node>::type
            {
                assert( node != nullptr );
                return dispatch_as<ReturnT>( node, *this, env );
            }

            template<typename Node, typename Env = environment_base>
            auto dispatch( std::shared_ptr<Node> const& node, std::shared_ptr<Env> const& env = nullptr ) const
                -> typename self_type::template result<Node>::type
            {
                return dispatch( std::const_pointer_cast<Node const>( node ), std::const_pointer_cast<Env const>( env ) );
            }

        private:
            detail::visitor_invoker<Derived, ReturnT> master_invoker_;
        };

    } // namespace ast
} // namespace rill


#endif /*RILL_AST_VISITOR_HPP*/
