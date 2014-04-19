//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <vector>
#include <string>
#include <functional>

#include "../environment/environment_fwd.hpp"
#include "../behavior/intrinsic_function_holder_fwd.hpp"

#include "ast_base.hpp"
#include "expression_fwd.hpp"

#include "value.hpp"


namespace rill
{
    namespace ast
    {
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        //
        // expressions
        //
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        RILL_AST_CORE_BEGIN( expression )
        RILL_AST_CORE_END



        struct binary_operator_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            binary_operator_expression(
                expression_ptr const& lhs,
                identifier_value_ptr const& op,
                expression_ptr const& rhs
                )
                : lhs_( lhs )
                , op_( op )
                , rhs_( rhs )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                binary_operator_expression,
                (( expression_ptr, lhs_ ))
                (( identifier_value_ptr, op_ ))
                (( expression_ptr, rhs_ ))
                )
        };


        // will be used by Array, Range, Slice...
        struct subscrpting_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            subscrpting_expression(
                expression_ptr const& lhs,
                boost::optional<expression_ptr> const& rhs
                )
                : lhs_( lhs )
                , rhs_( rhs )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                subscrpting_expression,
                (( expression_ptr, lhs_ ))
                (( boost::optional<expression_ptr>, rhs_,
                   if ( rhs_ )
                       cloned->rhs_ = clone_ast( *rhs_ );
                    ))
                )
        };


        //
        struct element_selector_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            element_selector_expression(
                expression_ptr const& reciever,
                identifier_value_base_ptr const& selector_id
                )
                : reciever_( reciever )
                , selector_id_( selector_id )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                element_selector_expression,
                (( expression_ptr, reciever_ ))
                (( identifier_value_base_ptr, selector_id_ ))
                )
        };




        struct call_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            call_expression(
                expression_ptr const& reciever,
                expression_list const& arguments
                )
                : reciever_( reciever )
                , arguments_( arguments )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                call_expression,
                (( expression_ptr, reciever_ ))
                (( expression_list, arguments_,
                   for( auto const& e : arguments_ )
                       cloned->arguments_.push_back( clone_ast( e ) );
                    ))
                )
        };







        //
        struct intrinsic_function_call_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            intrinsic_function_call_expression( intrinsic_function_action_id_t const& action_id )
                : action_id_( action_id )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                intrinsic_function_call_expression,
                (( intrinsic_function_action_id_t, action_id_,
                   cloned->action_id_ = action_id_;
                    ))
                )
        };



        //
        //
        //
        struct type_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            type_expression( expression_ptr const& type )
                : type_( type )
            {}

            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                type_expression,
                (( expression_ptr, type_ ))
                )
        };



        struct term_expression RILL_CXX11_FINAL
            : public expression
        {
        public:
            term_expression( value_ptr const& v )
                : value_( v )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                term_expression,
                (( value_ptr, value_ ))
                )
        };

    } // namespace ast
} // namespace rill
