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
#include <typeinfo>

#include <boost/core/demangle.hpp>

#include "../../config/macros.hpp"
#include "../../message/message.hpp"

#include "../../environment/environment_fwd.hpp"

#include "macros_for_visitor.hpp"
#include "visitor_delegator.hpp"
#include "tree_visitor_result_t.hpp"
#include "tree_visitor_default_value.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // base class of ast visitors
            template<typename Derived, typename ReturnT>
            struct tree_visitor_base
            {
            public:
                typedef tree_visitor_base           self_type;
                typedef self_type const             const_self_type;

                template<typename NodeT>
                using result_type = tree_visitor_result_t<ReturnT, NodeT>;

                visitor_delegator<Derived, ReturnT> delegator_;

            public:
                tree_visitor_base()
                    : delegator_( static_cast<Derived*>( this ) )
                {}

                virtual ~tree_visitor_base() {};

            public:
                template<typename... Args>
                auto send_error( Args&&... args ) const
                    -> void
                {
                    assert( false );

                    throw message::message_object(
                        message::message_level::e_error,
                        std::forward<Args>( args )...
                        );
                }

                template<typename... Args>
                auto send_warning( Args&&... args ) const
                    -> void
                {
                    assert( false );

                    // TODO: see compile switch
                    save_message(
                        message::message_object(
                            message::message_level::e_warning,
                            std::forward<Args>( args )...
                            )
                        );
                }

                template<typename... Args>
                auto send_message( Args&&... args ) const
                    -> void
                {
                    assert( false );

                    save_message(
                        message::message_object(
                            message::message_level::e_normal,
                            std::forward<Args>( args )...
                            )
                        );
                }

                auto has_message() const
                    -> bool
                {
                    return !messages_.empty();
                }

                auto has_error() const
                    -> bool
                {
                    return false;       // TODO: implement
                }

            protected:
                // called from derived visitor
                template<typename Node, typename Env>
                auto invoke( std::shared_ptr<Node> const& node, std::shared_ptr<Env> const& env )
                    -> result_type<Node>
                try {
                    //
                    char storage[sizeof(std::conditional_t<!std::is_same<result_type<Node>, void>::value, result_type<Node>, char/*temp*/>)];
                    node->dispatch(
                        node,
                        delegator_,
                        env,
                        static_cast<void*>( storage )
                        );

                    return make_return_value<result_type<Node>>{}( storage );

                } catch( message::message_object const& e ) {
                    save_message( e );
                    return make_default_return_value<result_type<Node>>{}();       // implies failed...
                }

                template<typename Node, typename Env>
                auto invoke( std::shared_ptr<Node> const& node, std::shared_ptr<Env> const& env ) const
                    -> result_type<Node>
                try {
                    char storage[sizeof(std::conditional_t<!std::is_same<result_type<Node>, void>::value, result_type<Node>, char/*temp*/>)];
                    node->dispatch(
                        node,
                        delegator_,
                        env,
                        static_cast<void*>( storage )
                        );

                    return make_return_value<result_type<Node>>{}( storage );

                } catch( message::message_object const& e ) {
                    save_message( e );
                    return make_default_return_value<result_type<Node>>{}();       // implies failed...
                }

                template<typename Node>
                auto failed_to_dispatch()
                    -> result_type<Node>
                {
                    std::cerr
                        << "!!! DEBUG: this AST node was not implemented" << std::endl
                        << "VISITOR -> " << boost::core::demangle( typeid( *this ).name() ) << std::endl
                        << "AST     -> " << boost::core::demangle( typeid( Node* ).name() ) << " / is_const: " << std::is_const<Node>::value << std::endl;

                    return make_default_return_value<result_type<Node>>{}();
                }

                template<typename Node>
                auto failed_to_dispatch() const
                    -> result_type<Node>
                {
                    std::cerr
                        << "!!! DEBUG: this AST node was not implemented" << std::endl
                        << "VISITOR -> " << boost::core::demangle( typeid( *this ).name() ) << " / const" << std::endl
                        << "AST     -> " << boost::core::demangle( typeid( Node* ).name() ) << " / is_const: " << std::is_const<Node>::value << std::endl;

                    return make_default_return_value<result_type<Node>>{}();
                }

            private:
                template<typename T>
                auto save_message( T&& m ) const
                    -> void
                {
                    messages_.push_back( std::forward<T>( m ) );
                }

            private:
                mutable std::vector<message::message_object> messages_;
            };

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP*/
