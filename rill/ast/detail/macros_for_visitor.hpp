//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//


#define RILL_VISITOR_OP_INDIRECT( node_type, node_name, env_name, result_scope, prefix ) \
    typename result_scope result_type<node_type> \
    prefix operator()( \
        std::shared_ptr<node_type> const& node_name, \
        rill::environment_base_ptr const& env_name   \
        )

#define RILL_VISITOR_READONLY_OP_INDIRECT( node_type, node_name, env_name, result_scope, prefix ) \
    typename result_scope result_type<node_type> \
    prefix operator()( \
        std::shared_ptr<node_type const> const& node_name, \
        rill::const_environment_base_ptr const& env_name   \
        )


#define RILL_VISITOR_OP( class_name, node_type, node_name, env_name ) \
    RILL_VISITOR_OP_INDIRECT( node_type, node_name, env_name, class_name::, class_name:: )

#define RILL_VISITOR_READONLY_OP( class_name, node_type, node_name, env_name ) \
    RILL_VISITOR_READONLY_OP_INDIRECT( node_type, node_name, env_name, class_name::, class_name:: )


#define RILL_VISITOR_OP_DECL( node_type ) \
    RILL_VISITOR_OP_INDIRECT( node_type, , , self_type::template, )

#define RILL_VISITOR_READONLY_OP_DECL( node_type ) \
    RILL_VISITOR_READONLY_OP_INDIRECT( node_type, , , self_type::template, )


#define RILL_VISITOR_OP_DECL_INNER( node_type, node_name, env_name ) \
    RILL_VISITOR_OP_INDIRECT( node_type, node_name, env_name, self_type::template, )

#define RILL_VISITOR_READONLY_OP_DECL_INNER( node_type, node_name, env_name ) \
    RILL_VISITOR_READONLY_OP_INDIRECT( node_type, node_name, env_name, self_type::template, )


//
#define RILL_VISITOR_OP_DEFAULT                 \
    using self_type::base_type::operator();
