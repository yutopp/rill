//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_TREE_FILTER_HPP
#define RILL_AST_DETAIL_TREE_FILTER_HPP

#include <utility>

#include "tree_visitor_base.hpp"
#include "../ast.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename MatchNode, typename F>
            class node_filter RILL_CXX11_FINAL
                : public tree_visitor<node_filter<MatchNode, F>, typename std::result_of<F(std::shared_ptr<MatchNode> const&, environment_base_ptr const&)>::type>
            {
            public:
                template<typename NodeT>
                struct result
                {
                    typedef typename ast::detail::tree_visitor_result<
                        typename std::result_of<F(std::shared_ptr<MatchNode> const&, environment_base_ptr const&)>::type,
                        typename ast::detail::base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                node_filter( F const& f )
                    : f_( f )
                {}

            public:
                RILL_TV_OP_INDIRECT( ast::statements, s, _ )
                {
                    for( auto const& ss : s->statement_list_ )
                        this->dispatch( ss, _ );
                }

                RILL_TV_OP_INDIRECT( ast::block_statement, s, _ )
                {
                    //
                    this->dispatch( s->statements_, _ );
                }

                RILL_TV_OP_INDIRECT( MatchNode, node, _ )
                {
                    //
                    return f_( node, _ );
                }

                RILL_TV_OP_FAIL

                template<typename NodeT>
                auto failed_to_dispatch() const
                    -> void
                {}

            private:
                F const& f_;
            };



            template<typename MatchNode, typename F>
            class const_node_filter RILL_CXX11_FINAL
                : public const_tree_visitor<const_node_filter<MatchNode, F>, typename std::result_of<F(std::shared_ptr<MatchNode const> const&, const_environment_base_ptr const&)>::type>
            {
            public:
                template<typename NodeT>
                struct result
                {
                    typedef typename ast::detail::tree_visitor_result<
                        typename std::result_of<F(std::shared_ptr<MatchNode const> const&, const_environment_base_ptr const&)>::type,
                        typename ast::detail::base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                const_node_filter( F const& f )
                    : f_( f )
                {}

            public:
                RILL_TV_OP_INDIRECT_CONST( ast::statements, s, _ )
                {
                    for( auto const& ss : s->statement_list_ )
                        this->dispatch( ss, _ );
                }

                RILL_TV_OP_INDIRECT_CONST( ast::block_statement, s, _ )
                {
                    //
                    this->dispatch( s->statements_, _ );
                }

                RILL_TV_OP_INDIRECT_CONST( MatchNode, node, _ )
                {
                    //
                    return f_( node, _ );
                }

                RILL_TV_OP_FAIL

                template<typename NodeT>
                auto failed_to_dispatch() const
                    -> void
                {}

            private:
                F const& f_;
            };



            template<typename MatchNode, typename Node, typename Env, typename F>
            auto apply_to_node( Node const& node, Env const& env, F const& f )
                -> decltype( node_filter<MatchNode, F>( f ).dispatch( node, env ) )
            {
                node_filter<MatchNode, F> filter( f );

                return filter.dispatch( node, env );
            }

            template<typename MatchNode, typename Node, typename Env, typename F>
            auto apply_to_const_node( Node const& node, Env const& env, F const& f )
                -> decltype( const_node_filter<MatchNode, F>( f ).dispatch( node, env ) )
            {
                const_node_filter<MatchNode, F> const filter( f );

                return filter.dispatch( node, env );
            }

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_TREE_FILTER_HPP*/
