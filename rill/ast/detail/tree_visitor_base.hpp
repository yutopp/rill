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

#include "../../config/macros.hpp"

#include "../../environment/environment_fwd.hpp"

#include "../value_fwd.hpp"
#include "../expression_fwd.hpp"
#include "../statement_fwd.hpp"

#include "dispatch_functions.hpp"


#define RILL_TV_OP_INDIRECT( node_type, node_name, env_name ) \
    auto operator()( std::shared_ptr<node_type> const& node_name, environment_base_ptr const& env_name ) \
        -> typename result<node_type>::type

#define RILL_TV_OP_INDIRECT_CONST( node_type, node_name, env_name ) \
    auto operator()( std::shared_ptr<node_type const> const& node_name, const_environment_base_ptr const& env_name ) const \
        -> typename result<node_type>::type

// filter outbound object
#define RILL_TV_OP_FAIL \
    template<typename NodeT> \
    auto operator()( std::shared_ptr<NodeT> const& node_name, environment_base_ptr const& env_name ) \
        -> typename result<NodeT>::type \
    { \
        this->template failed_to_dispatch<NodeT>();   \
        return typename result<NodeT>::type(); \
    } \
    template<typename NodeT> \
    auto operator()( std::shared_ptr<NodeT const> const& node_name, const_environment_base_ptr const& env_name ) const \
        -> typename result<NodeT>::type \
    { \
        this->template failed_to_dispatch<NodeT const>();   \
        return typename result<NodeT>::type(); \
    }
    


#define RILL_TV_OP_DECL( node_type ) RILL_TV_OP_INDIRECT( node_type, , );

#define RILL_TV_OP_DECL_CONST( node_type ) RILL_TV_OP_INDIRECT_CONST( node_type, , );


#define RILL_TV_OP( class_name, node_type, node_name, env_name ) \
    auto class_name::operator()( std::shared_ptr<node_type> const& node_name, environment_base_ptr const& env_name ) \
        -> typename result<node_type>::type

#define RILL_TV_OP_CONST( class_name, node_type, node_name, env_name ) \
    auto class_name::operator()( std::shared_ptr<node_type const> const& node_name, const_environment_base_ptr const& env_name ) const \
        -> typename result<node_type>::type






///
#define RILL_TV_INVOKER_VOID_OP_VIRTUAL( node_type ) \
    virtual typename result<node_type>::type operator()( std::shared_ptr<node_type> const&, environment_base_ptr const&, tree_visitor_base<ReturnT>* const ) =0; \
    virtual typename result<node_type>::type operator()( std::shared_ptr<node_type const> const&, const_environment_base_ptr const&, tree_visitor_base<ReturnT> const* const ) const =0;


#define RILL_TV_INVOKER_RETURN_OP_VIRTUAL( node_type ) \
    virtual auto operator()( std::shared_ptr<node_type> const&, environment_base_ptr const&, tree_visitor_base<ReturnT>* const ) \
        -> typename result<node_type>::type =0; \
    virtual auto operator()( std::shared_ptr<node_type const> const& node, const_environment_base_ptr const& env, tree_visitor_base<ReturnT> const* const ) const \
        -> typename result<node_type>::type =0; \


///
#define RILL_TV_INVOKER_VOID_OP( node_type ) \
    void operator()( std::shared_ptr<node_type> const& node, environment_base_ptr const& env, tree_visitor_base<ReturnT>* const v ) RILL_CXX11_OVERRIDE \
    { \
        static_cast<Visitor&>( *v )( node, env ); \
    } \
    void operator()( std::shared_ptr<node_type const> const& node, const_environment_base_ptr const& env, tree_visitor_base<ReturnT> const* const v ) const RILL_CXX11_OVERRIDE \
    { \
        static_cast<Visitor const&>( *v )( node, env ); \
    } \


#define RILL_TV_INVOKER_RETURN_OP( node_type ) \
    auto operator()( std::shared_ptr<node_type> const& node, environment_base_ptr const& env, tree_visitor_base<ReturnT>* const v ) \
        -> typename result<node_type>::type RILL_CXX11_OVERRIDE \
    { \
        return static_cast<Visitor&>( *v )( node, env );  \
    } \
    auto operator()( std::shared_ptr<node_type const> const& node, const_environment_base_ptr const& env, tree_visitor_base<ReturnT> const* const v ) const \
        -> typename result<node_type>::type RILL_CXX11_OVERRIDE \
    { \
        return static_cast<Visitor const&>( *v )( node, env ); \
    } \


#include <boost/preprocessor.hpp>

#define RILL_TV_ALL_AST_LIST() \
((statement, VOID)) \
((statements, VOID)) \
((block_statement, VOID)) \
((can_be_template_statement, VOID)) \
((template_statement, VOID)) \
((empty_statement, VOID)) \
((expression_statement, VOID)) \
((function_definition_statement_base, VOID)) \
((function_definition_statement, VOID)) \
((intrinsic_function_definition_statement, VOID)) \
((class_function_definition_statement, VOID)) \
((class_definition_statement, VOID)) \
((return_statement, VOID)) \
((jit_statement, VOID)) \
((extern_statement_base, VOID)) \
((extern_function_declaration_statement, VOID)) \
((variable_declaration_statement, VOID)) \
((class_variable_declaration_statement, VOID)) \
((test_while_statement, VOID)) \
((test_if_statement, VOID)) \
\
((expression, RETURN)) \
((binary_operator_expression, RETURN)) \
((element_selector_expression, RETURN)) \
((subscrpting_expression, RETURN)) \
((call_expression, RETURN)) \
((intrinsic_function_call_expression, RETURN)) \
((type_expression, RETURN)) \
((term_expression, RETURN)) \
\
((value, RETURN)) \
((intrinsic::value_base, RETURN)) \
((intrinsic::symbol_value, RETURN)) \
((intrinsic::int32_value, RETURN)) \
((intrinsic::boolean_value, RETURN)) \
((intrinsic::string_value, RETURN)) \
((identifier_value_base, RETURN)) \
((identifier_value, RETURN)) \
((template_instance_value, RETURN)) \
((nested_identifier_value, RETURN)) \
((literal_value, RETURN))



#define RILL_TV_INVOKER_GEN_( r_type, node ) \
    RILL_TV_INVOKER_ ## r_type ( node )

#define RILL_TV_INVOKER_GEN( r_type, node ) \
    RILL_TV_INVOKER_GEN_( r_type, ast :: node )

#define RILL_TV_INVOKER_OP_VIATURL_GEN( r, _, elem_tuple ) \
    RILL_TV_INVOKER_GEN( \
        BOOST_PP_CAT( BOOST_PP_TUPLE_ELEM( 2, 1, elem_tuple ), _OP_VIRTUAL ), \
        BOOST_PP_TUPLE_ELEM( 2, 0, elem_tuple ) \
        )


#define RILL_TV_INVOKER_OP_GEN( r, _, elem_tuple ) \
    RILL_TV_INVOKER_GEN( \
        BOOST_PP_CAT( BOOST_PP_TUPLE_ELEM( 2, 1, elem_tuple ), _OP ), \
        BOOST_PP_TUPLE_ELEM( 2, 0, elem_tuple ) \
        )



namespace rill
{
    namespace ast
    {
        namespace detail
        {
            // --
            template<typename R, typename BaseNode>
            struct tree_visitor_result;

            template<typename R> struct tree_visitor_result<R, ast::statement>  { typedef void type; };
            template<typename R> struct tree_visitor_result<R, ast::expression> { typedef R type; };
            template<typename R> struct tree_visitor_result<R, ast::value>      { typedef R type; };
            // --


            //
            template<typename ReturnT>
            struct tree_visitor_base;


            // TODO: reuse
            template<typename ReturnT, typename NodeT>
            struct visitor_result_traits
            {
                typedef typename tree_visitor_result<
                    ReturnT,
                    typename base_type_specifier<typename std::decay<NodeT>::type>::type
                >::type type;
            };


            //
            template<typename ReturnT>
            class visitor_invoker_base
            {
            public:
                template<typename NodeT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                //
                BOOST_PP_SEQ_FOR_EACH( RILL_TV_INVOKER_OP_VIATURL_GEN, _,  RILL_TV_ALL_AST_LIST() )
            };






            template<typename Visitor, typename ReturnT>
            class visitor_invoker
                : public visitor_invoker_base<ReturnT>
            {
            public:
                template<typename NodeT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                BOOST_PP_SEQ_FOR_EACH( RILL_TV_INVOKER_OP_GEN, _,  RILL_TV_ALL_AST_LIST() )
            };


            // forward
            template<typename ReturnT, typename Derived>
            struct tree_visitor;
            template<typename ReturnT, typename Derived>
            struct const_tree_visitor;

            // --
            // base class of ast visitor
            // --
            template<typename ReturnT>
            struct tree_visitor_base
            {
            public:
                template<typename, typename>
                friend struct tree_visitor;
                template<typename, typename>
                friend struct const_tree_visitor;

            public:
                typedef tree_visitor_base           self_type;
                typedef self_type const             const_self_type;

                template<typename NodeT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                tree_visitor_base( visitor_invoker_base<ReturnT>& invoker )
                    : invoker_( std::ref( invoker ) )
                {}

                virtual ~tree_visitor_base() {};

            public:
                //
                template<typename Node>
                auto invoke( std::shared_ptr<Node> const& node, environment_base_ptr const& env )
                    -> typename result<Node>::type
                {
                    return invoker_( node, env, this );
                }

                //
                template<typename Node>
                auto invoke( std::shared_ptr<Node const> const& node, const_environment_base_ptr const& env ) const
                    -> typename result<Node>::type
                {
                    return invoker_( node, env, this );
                }
                
            public:
                template<typename NodeT>
                auto failed_to_dispatch() const
                    -> void
                {
                    std::cerr
                        << "!!! DEBUG: this AST node was not implemented" << std::endl
                        << "VISITOR -> " << typeid( *this ).name() << " / is_const: " << std::is_const<decltype( *this )>::value << std::endl
                        << "AST     -> " << typeid( NodeT ).name() << " / is_const: " << std::is_const<NodeT>::value << std::endl;
                        ;
                }

            private:
                std::reference_wrapper<visitor_invoker_base<ReturnT>> invoker_;
            };



            // --
            // base class of ast visitor( NON Copyable )
            // --
            template<typename Derived, typename ReturnT>
            struct tree_visitor
                : public tree_visitor_base<ReturnT>
            {
            public:
                typedef tree_visitor                self_type;
                typedef self_type const             const_self_type;

                typedef tree_visitor_base<ReturnT> base_type;

                template<typename NodeT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                tree_visitor()
                    : base_type( std::ref( master_invoker_ ) )
                {}

                virtual ~tree_visitor() {}

            public:
                RILL_TV_OP_FAIL

            public:
                //
                template<typename Node, typename Enveronment = environment_base, typename = typename std::enable_if<(!std::is_const<Node>::value)>::type>
                auto dispatch( std::shared_ptr<Node> const& node, std::shared_ptr<Enveronment> const& env = nullptr )
                    -> decltype( dispatch_as<ReturnT>( node, *reinterpret_cast<self_type*>(0), env ) )
                {
                    return dispatch_as<ReturnT>(
                        node,
                        *this,
                        env
                        );
                }

            public:
                //
                template<typename Node, typename Enveronment = environment_base>
                auto dispatch_as_terminal( std::shared_ptr<Node> const& node, std::shared_ptr<Enveronment> const& env = nullptr )
                    -> bool
                try {
                    dispatch( node, env );
                    return true;
                }
                catch(...) {
                    return false;
                }

            private:
                visitor_invoker<Derived, ReturnT> master_invoker_;
            };



            // --
            // base class of ast visitor( NON Copyable )
            // --
            template<typename Derived, typename ReturnT>
            struct const_tree_visitor
                : public tree_visitor_base<ReturnT>
            {
            public:
                typedef const_tree_visitor          self_type;
                typedef self_type const             const_self_type;

                typedef tree_visitor_base<ReturnT> base_type;

                template<typename NodeT>
                struct result
                {
                    typedef typename tree_visitor_result<
                        ReturnT,
                        typename base_type_specifier<typename std::decay<NodeT>::type>::type
                    >::type type;
                };

            public:
                const_tree_visitor()
                    : base_type( std::ref( master_invoker_ ) )
                {}

                virtual ~const_tree_visitor() {}

            public:
                RILL_TV_OP_FAIL

            public:
                //
                template<typename Node, typename Enveronment = environment_base>
                auto dispatch( std::shared_ptr<Node> const& node, std::shared_ptr<Enveronment> const& env = nullptr ) const
                    -> decltype( dispatch_as<ReturnT>( std::const_pointer_cast<Node const>( node ), *reinterpret_cast<const_self_type*>(0), std::static_pointer_cast<environment_base const>( env ) ) )
                {
                    auto const& p = std::const_pointer_cast<Node const>( node );
                    assert( p != nullptr );

                    return dispatch_as<ReturnT>(
                        p,
                        *this,
                        std::static_pointer_cast<environment_base const>( env )
                        );
                }

            public:
                //
                template<typename Node, typename Enveronment = environment_base>
                auto dispatch_as_terminal( std::shared_ptr<Node> const& node, std::shared_ptr<Enveronment> const& env = nullptr ) const
                    -> bool
                try {
                    dispatch( node, env );
                    return true;
                }
                catch(...) {
                    return false;
                }

            private:
                visitor_invoker<Derived, ReturnT> master_invoker_;
            };



        } // namespace detail
    } // namespace ast
} // namespace rill

#undef RILL_TV_BASE_VOID_OP
#undef RILL_TV_BASE_RETURN_OP


#endif /*RILL_AST_DETAIL_TREE_VISITOR_BASE_HPP*/
