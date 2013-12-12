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

#include "detail/tree_visitor_base.hpp"
#include "detail/dispatch_assets.hpp"

#include "expression_fwd.hpp"

#include "ast_base.hpp"
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
        struct expression
            : public ast_base
        {
        public:
            RILL_AST_ADAPT_VISITOR( expression )

        public:
            virtual ~expression() {}
        };


        typedef std::vector<expression_ptr>     expression_list;



        struct binary_operator_expression
            : public expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( binary_operator_expression )

        public:
            binary_operator_expression(
                expression_ptr const& lhs,
                intrinsic::identifier_value_ptr const& op,
                expression_ptr const& rhs
                )
                : lhs_( lhs )
                , op_( op )
                , rhs_( rhs )
            {}

        public:
            expression_ptr const lhs_;
            intrinsic::identifier_value_ptr const op_;
            expression_ptr const rhs_;
        };


        struct call_expression
            : public expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( call_expression )

        public:
            call_expression(
                intrinsic::identifier_value_base_ptr/*expression_ptr*/ const& reciever,
                expression_list const& arguments
                )
                : reciever_( reciever )
                , arguments_( arguments )
            {}

        public:
            intrinsic::identifier_value_base_ptr/*expression_ptr*/ const reciever_;
            expression_list const arguments_;
        };


        //
        struct intrinsic_function_call_expression
            : public expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( intrinsic_function_call_expression )

        public:
            intrinsic_function_call_expression( intrinsic_function_action_id_t const& action_id )
                : action_id_( action_id )
            {}

        public:
            intrinsic_function_action_id_t const action_id_;
        };



        struct term_expression
            : public expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( term_expression )

        public:
            term_expression( value_ptr const& v )
                : value_( v )
            {}

        public:
            value_ptr const value_;
        };



        struct type_expression
            : public expression
        {
            RILL_AST_ADAPT_VISITOR( type_expression );
            //ast_base_type a;
            //ADAPT_EXPRESSION_VISITOR( type_expression )

        public:
        };


        //
        struct type_identifier_expression
            : public type_expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( type_identifier_expression )

        public:
            type_identifier_expression(
                intrinsic::nested_identifier_value_ptr const& v,
                attribute::type_attributes_optional const& attributes
                )
                : value_( v )
                , attributes_( attributes )
            {}

        public:
            intrinsic::nested_identifier_value_ptr const value_;
            attribute::type_attributes_optional const attributes_;
        };



        //
        struct compiletime_return_type_expression
            : public type_expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( compiletime_return_type_expression )

        public:
            compiletime_return_type_expression( expression_ptr const& e )
                : expression_( e )
            {}

        public:
            expression_ptr expression_;
        };

    } // namespace ast
} // namespace rill
