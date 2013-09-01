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

#include "detail/tree_visitor_base.hpp"
#include "detail/dispatch_assets.hpp"

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
        struct expression
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
            binary_operator_expression( expression_ptr const& lhs, intrinsic::single_identifier_value_ptr const& op, expression_ptr const& rhs )
                : lhs_( lhs )
                , op_( op )
                , rhs_( rhs )
            {}

        public:
            expression_ptr const lhs_;
            intrinsic::single_identifier_value_ptr const op_;
            expression_ptr const rhs_;
        };


        struct call_expression
            : public expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( call_expression )

        public:
            call_expression( intrinsic::identifier_value_ptr const& caller, expression_list const& arguments )
                : reciever_( caller )
                , arguments_( arguments )
            {}

        public:
            intrinsic::identifier_value_ptr const reciever_;
            expression_list const arguments_;
        };


        //
        typedef std::function<intrinsic::value_base_ptr (std::vector<const_value_ptr> const&)> embedded_callback_function_t;

        struct embedded_function_call_expression
            : public expression
        {
        public:
            RILL_AST_ADAPT_VISITOR( embedded_function_call_expression )

        public:
            embedded_function_call_expression( embedded_callback_function_t const& reciever )
                : reciever_( reciever )
            {}

        public:
            embedded_callback_function_t const reciever_;
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
            type_identifier_expression( intrinsic::identifier_value_ptr const& v )
                : value_( v )
            {}

        public:
            intrinsic::identifier_value_ptr const value_;
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
