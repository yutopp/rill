//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP
#define RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP

#include <iostream>
#include <cassert>  // for assert

#include "../../config/macros.hpp"

#include "../../environment_fwd.hpp"

#include "../value_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../statement_fwd.hpp"
#include "../root_fwd.hpp"

#include "dispatch_functions.hpp"


#define RILL_TV_OP_DECL( node_pointer_type ) \
    auto operator()( node_pointer_type const&, environment_ptr const& ) const \
        -> result<node_pointer_type::element_type>::type RILL_CXX11_OVERRIDE;

#define RILL_TV_OP( class_name, node_pointer_type, node_name, env_name ) \
    auto class_name::operator()( node_pointer_type const& node_name, environment_ptr const& env_name ) const \
        -> result<node_pointer_type::element_type>::type


///
#define RILL_TV_BASE_VOID_OP( node_pointer_type ) \
    virtual void operator()( node_pointer_type const& node, environment_ptr const& env ) const \
    { \
        this->unimplemented<node_pointer_type>(); \
    }

#define RILL_TV_BASE_RETURN_OP( node_pointer_type ) \
    virtual auto operator()( node_pointer_type const& node, environment_ptr const& env ) const \
        -> typename result<node_pointer_type::element_type>::type \
    { \
        this->unimplemented<node_pointer_type::element_type>(); \
        return nullptr; \
    }


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            template<typename R, typename BaseNode>
            struct tree_visitor_result;

            template<typename R> struct tree_visitor_result<R, ast::root>       { typedef void type; };
            template<typename R> struct tree_visitor_result<R, ast::statement>  { typedef void type; };
            template<typename R> struct tree_visitor_result<R, ast::expression> { typedef R type; };
            template<typename R> struct tree_visitor_result<R, ast::value>      { typedef R type; };


            // --
            // base class of ast visitor
            // --
            template<typename ReturnT>
            struct tree_visitor_base
            {
            public:
                template<typename NodePtrT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<NodePtrT>::type
                    >::type type;
                };

            public:
                virtual ~tree_visitor_base() {}

            public:
                //
                RILL_TV_BASE_VOID_OP( ast::root_ptr )

                // statement
                // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;
                RILL_TV_BASE_VOID_OP( ast::expression_statement_ptr )
                RILL_TV_BASE_VOID_OP( ast::return_statement_ptr )
                RILL_TV_BASE_VOID_OP( ast::function_definition_statement_ptr )
                RILL_TV_BASE_VOID_OP( ast::class_definition_statement_ptr )

                // expression
                RILL_TV_BASE_RETURN_OP( ast::binary_operator_expression_ptr )
                RILL_TV_BASE_RETURN_OP( ast::call_expression_ptr )
                RILL_TV_BASE_RETURN_OP( ast::embedded_function_call_expression_ptr )
                RILL_TV_BASE_RETURN_OP( ast::term_expression_ptr )
                RILL_TV_BASE_RETURN_OP( ast::type_identifier_expression_ptr )
                RILL_TV_BASE_RETURN_OP( ast::compiletime_return_type_expression_ptr )

                // value
                RILL_TV_BASE_RETURN_OP( ast::intrinsic_value_ptr )
                RILL_TV_BASE_RETURN_OP( ast::variable_value_ptr )

            public:
                // filter outbound object
                template<typename NodeT>
                auto operator()( std::shared_ptr<NodeT> const&, environment_ptr const& ) const
                    -> typename result<NodeT>::type
                {
                    unimplemented<NodeT>();
                    return typename result<NodeT>::type();
                }

            private:
                template<typename NodeT>
                auto unimplemented() const
                    -> void
                {
                    std::cerr
                        << "!!! DEBUG: message. please implement it!" << std::endl
                        << "in " << typeid( *this ).name() << std::endl
                        << " -> " << typeid( NodeT ).name() << std::endl;
                }
            };
        } // namespace detail
    } // namespace ast
} // namespace rill

#undef RILL_TV_BASE_VOID_OP
#undef RILL_TV_BASE_RETURN_OP

#endif /*RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP*/
