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
#include <typeinfo>
#include <type_traits>

#include <boost/core/demangle.hpp>

#include "../../config/macros.hpp"
#include "../../environment/environment_fwd.hpp"

#include "macros_for_visitor.hpp"
#include "visitor_delegator.hpp"
#include "tree_visitor_result_t.hpp"
#include "tree_visitor_default_value.hpp"
#include "../statement_fwd.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // base class of ast visitors
            template<typename Derived, typename ReturnT, typename Messaging>
            struct tree_visitor_base
                : public Messaging
            {
            private:
                using mc_type = Messaging;

            public:
                typedef tree_visitor_base               self_type;
                typedef self_type const                 const_self_type;

                template<typename NodeT>
                using result_type = tree_visitor_result_t<ReturnT, NodeT>;

            public:
                tree_visitor_base()
                    : delegator_( static_cast<Derived*>( this ) )
                {}

                virtual ~tree_visitor_base() {};

            protected:
                // called from derived visitor
                template<typename Node, typename Env>
                auto invoke( std::shared_ptr<Node> const& node, std::shared_ptr<Env> const& env )
                    -> result_type<Node>
                try {
                    // TODO: alignment
                    char storage[sizeof(std::conditional_t<!std::is_same<result_type<Node>, void>::value, result_type<Node>, char/*temp*/>)];
                    node->dispatch(
                        node,
                        delegator_,
                        env,
                        static_cast<void*>( storage )
                        );

                    return make_return_value<result_type<Node>>{}( storage );

                } catch( typename mc_type::message_type const& e ) {
                    if ( std::is_base_of<statement, Node>::value ) {
                        auto cp = e;    // copy
                        mc_type::save_message( std::move( cp ) );
                        mc_type::is_error_state( false );
                        return make_default_return_value<result_type<Node>>{}();       // implies failed...

                    } else {
                        throw;
                    }
                }

                template<typename Node, typename Env>
                auto invoke( std::shared_ptr<Node> const& node, std::shared_ptr<Env> const& env ) const
                    -> result_type<Node>
                try {
                    // TODO: alignment
                    char storage[sizeof(std::conditional_t<!std::is_same<result_type<Node>, void>::value, result_type<Node>, char/*temp*/>)];
                    node->dispatch(
                        node,
                        delegator_,
                        env,
                        static_cast<void*>( storage )
                        );

                    return make_return_value<result_type<Node>>{}( storage );

                } catch( typename mc_type::message_type const& e ) {
                    if ( std::is_base_of<statement, Node>::value ) {
                        auto cp = e;    // copy
                        mc_type::save_message( std::move( cp ) );
                        mc_type::is_error_state( false );
                        return make_default_return_value<result_type<Node>>{}();       // implies failed...

                    } else {
                        throw;
                    }
                }

                template<typename Node>
                auto failed_to_dispatch()
                    -> result_type<Node>
                {
                    debug_out << "!!! DEBUG: this AST node was not implemented" << std::endl
                              << "VISITOR -> " << boost::core::demangle( typeid( *this ).name() ) << std::endl
                              << "AST     -> " << boost::core::demangle( typeid( Node* ).name() ) << " / is_const: " << std::is_const<Node>::value << std::endl;

                    return make_default_return_value<result_type<Node>>{}();
                }

                template<typename Node>
                auto failed_to_dispatch() const
                    -> result_type<Node>
                {
                    debug_out << "!!! DEBUG: this AST node was not implemented" << std::endl
                              << "VISITOR -> " << boost::core::demangle( typeid( *this ).name() ) << " / const" << std::endl
                              << "AST     -> " << boost::core::demangle( typeid( Node* ).name() ) << " / is_const: " << std::is_const<Node>::value << std::endl;

                    return make_default_return_value<result_type<Node>>{}();
                }

            private:
                visitor_delegator<Derived, ReturnT> delegator_;
            };

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP*/
