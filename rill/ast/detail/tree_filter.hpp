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

#include "../visitor.hpp"
#include "../ast.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename MatchNode, typename F>
            class node_filter RILL_CXX11_FINAL
                : public ast_visitor_const<
                    node_filter<MatchNode, F>,
                    typename std::result_of<F(std::shared_ptr<MatchNode> const&, environment_base_ptr const&)>::type
                >
            {
            public:
                typedef node_filter         self_type;

            public:
                node_filter( F const& f )
                    : f_( f )
                {}

            public:
                RILL_VISITOR_OP_DECL_INNER( ast::statements, s, _ ) const
                {
                    for( auto const& ss : s->statement_list_ )
                        this->dispatch( ss, _ );
                }

                RILL_VISITOR_OP_DECL_INNER( ast::block_statement, s, _ ) const
                {
                    this->dispatch( s->statements_, _ );
                }

                //
                RILL_VISITOR_OP_DECL_INNER( MatchNode, node, _ ) const
                {
                    return f_( node, _ );
                }

                template<typename NodeT>
                auto failed_to_dispatch() const
                    -> void
                {}

                RILL_VISITOR_OP_FAIL

            private:
                F const& f_;
            };



            template<typename MatchNode, typename F>
            class readonly_node_filter RILL_CXX11_FINAL
                : public readonly_ast_visitor_const<
                    readonly_node_filter<MatchNode, F>,
                    typename std::result_of<F(std::shared_ptr<MatchNode const> const&, const_environment_base_ptr const&)>::type
                >
            {
            public:
                typedef readonly_node_filter    self_type;

            public:
                readonly_node_filter( F const& f )
                    : f_( f )
                {}

            public:
                RILL_VISITOR_READONLY_OP_DECL_INNER( ast::statements, s, _ ) const
                {
                    for( auto const& ss : s->statement_list_ )
                        this->dispatch( ss, _ );
                }

                RILL_VISITOR_READONLY_OP_DECL_INNER( ast::block_statement, s, _ ) const
                {
                    //
                    this->dispatch( s->statements_, _ );
                }

                RILL_VISITOR_READONLY_OP_DECL_INNER( MatchNode, node, _ ) const
                {
                    //
                    return f_( node, _ );
                }

                template<typename NodeT>
                auto failed_to_dispatch() const
                    -> void
                {}

                RILL_VISITOR_OP_FAIL

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
                -> decltype( readonly_node_filter<MatchNode, F>( f ).dispatch( node, env ) )
            {
                readonly_node_filter<MatchNode, F> filter( f );

                return filter.dispatch( node, env );
            }

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_TREE_FILTER_HPP*/
