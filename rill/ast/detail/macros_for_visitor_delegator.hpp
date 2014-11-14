//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_MACROS_FOR_VISITOR_DELEGATOR_HPP
#define RILL_AST_DETAIL_MACROS_FOR_VISITOR_DELEGATOR_HPP


#define RILL_VISITOR_DELEGATOR_SIGNATURE( node_type, qual ) \
    auto callback_from_node(                                \
        std::shared_ptr<node_type> const& node,             \
        environment_base_ptr const& env,                    \
        char* const storage                                 \
        ) qual                                              \
        -> char*

#define RILL_VISITOR_DELEGATOR_READONLY_SIGNATURE( node_type, qual )    \
    auto callback_from_node(                                            \
        std::shared_ptr<node_type const> const& node,                   \
        const_environment_base_ptr const& env,                          \
        char* const storage                                             \
        ) qual                                                          \
        -> char*


#define RILL_VISITOR_DELEGATOR_INTERFACE( node_type ) \
    virtual RILL_VISITOR_DELEGATOR_SIGNATURE( node_type, ) =0; \
    virtual RILL_VISITOR_DELEGATOR_SIGNATURE( node_type, const ) =0; \
    virtual RILL_VISITOR_DELEGATOR_READONLY_SIGNATURE( node_type, ) =0; \
    virtual RILL_VISITOR_DELEGATOR_READONLY_SIGNATURE( node_type, const ) =0;


#define RILL_VISITOR_DELEGATOR_DEFINITION( node_type )                  \
    RILL_VISITOR_DELEGATOR_SIGNATURE( node_type, ) override             \
    {                                                                   \
        return call_visitor<tree_visitor_result_t<ReturnT, node_type>>( \
            node,                                                       \
            env,                                                        \
            storage,                                                    \
            static_cast<tree_visitor_result_t<ReturnT, node_type>*>( nullptr ) \
            );                                                          \
    }                                                                   \
    RILL_VISITOR_DELEGATOR_SIGNATURE( node_type, const ) override       \
    {                                                                   \
        return call_visitor<tree_visitor_result_t<ReturnT, node_type>>( \
            node,                                                       \
            env,                                                        \
            storage,                                                    \
            static_cast<tree_visitor_result_t<ReturnT, node_type>*>( nullptr ) \
            );                                                          \
    }                                                                   \
    RILL_VISITOR_DELEGATOR_READONLY_SIGNATURE( node_type, ) override    \
    {                                                                   \
        return call_visitor<tree_visitor_result_t<ReturnT, node_type>>( \
            node,                                                       \
            env,                                                        \
            storage,                                                    \
            static_cast<tree_visitor_result_t<ReturnT, node_type>*>( nullptr ) \
            );                                                          \
    }                                                                   \
    RILL_VISITOR_DELEGATOR_READONLY_SIGNATURE( node_type, const ) override \
    {                                                                   \
        return call_visitor<tree_visitor_result_t<ReturnT, node_type>>( \
            node,                                                       \
            env,                                                        \
            storage,                                                    \
            static_cast<tree_visitor_result_t<ReturnT, node_type>*>( nullptr ) \
            );                                                          \
    }

#endif /*RILL_AST_DETAIL_MACROS_FOR_VISITOR_DELEGATOR_HPP*/
