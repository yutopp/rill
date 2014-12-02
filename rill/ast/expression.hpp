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
#include <boost/optional.hpp>

#include "../environment/environment_fwd.hpp"
#include "../type/type_id.hpp"

#include "ast_base.hpp"
#include "expression_fwd.hpp"
#include "statement_fwd.hpp"
#include "elements.hpp"

#include "value.hpp"


namespace rill
{
    namespace ast
    {
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        // expressions
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        RILL_AST_GROUP_BEGIN( expression )
        RILL_AST_GROUP_END


        RILL_AST_BEGIN(
            binary_operator_expression, expression,
            (( expression_ptr, lhs_ ))
            (( identifier_value_ptr, op_ ))
            (( expression_ptr, rhs_ ))
            )
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
        RILL_AST_END


        RILL_AST_BEGIN(
            unary_operator_expression, expression,
            (( identifier_value_ptr, op ))
            (( expression_ptr, src ))
            (( bool, is_prefix ))
            )
        public:
            unary_operator_expression(
                identifier_value_ptr const& o,
                expression_ptr const& s,
                bool const& i
                )
                : op( o )
                , src( s )
                , is_prefix( i )
            {}
        RILL_AST_END


        // will be used by Array, Range, Slice...
        RILL_AST_BEGIN(
            subscrpting_expression, expression,
            (( expression_ptr, lhs_ ))
            (( boost::optional<expression_ptr>, rhs_ ))
            )
        public:
            subscrpting_expression(
                expression_ptr const& lhs,
                boost::optional<expression_ptr> const& rhs
                )
                : lhs_( lhs )
                , rhs_( rhs )
            {}
        RILL_AST_END


        //
        RILL_AST_BEGIN(
            element_selector_expression, expression,
            (( expression_ptr, reciever_ ))
            (( identifier_value_base_ptr, selector_id_ ))
            )
        public:
            element_selector_expression(
                expression_ptr const& reciever,
                identifier_value_base_ptr const& selector_id
                )
                : reciever_( reciever )
                , selector_id_( selector_id )
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            call_expression, expression,
            (( expression_ptr, reciever_ ))
            (( expression_list, arguments_ ))
            )
        public:
            call_expression(
                expression_ptr reciever,
                expression_list arguments
                )
                : reciever_( std::move( reciever ) )
                , arguments_( std::move( arguments ) )
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            dereference_expression, expression,
            (( expression_ptr, reciever ))
            )
        public:
            dereference_expression(
                expression_ptr const& r
                )
                : reciever( r )
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            addressof_expression, expression,
            (( expression_ptr, reciever ))
            )
        public:
            addressof_expression(
                expression_ptr const& r
                )
                : reciever( r )
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            term_expression, expression,
            (( value_ptr, value_ ))
            )
        public:
            term_expression( value_ptr const& v )
                : value_( v )
            {}

            template<typename Ptr>
            inline auto after_constructing( Ptr const& self )
                -> void
            {
                self->value_->parent_expression = self;
            }
        RILL_AST_END


        RILL_AST_BEGIN(
            lambda_expression, expression,
            (( boost::optional<parameter_list>, template_parameters ))
            (( parameter_list, parameters ))
            (( element::statement_list, statements ))
            (( call_expression_ptr, call_expr ))
            )
        public:
            lambda_expression(
                boost::optional<parameter_list>&& tpl,
                parameter_list const& pl,
                element::statement_list const& ss
                )
                : template_parameters( std::move( tpl ) )
                , parameters( pl )
                , statements( ss )
                , call_expr( nullptr )  // assigned later
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            id_expression, expression,
            (( expression_ptr, expression_ ))
            )
        public:
            id_expression( expression_ptr const& expr )
                : expression_( expr )
            {}

        RILL_AST_END

        namespace helper
        {
            inline auto make_id_expression( identifier_value_base_ptr const& id )
                -> std::shared_ptr<id_expression>
            {
                return std::make_shared<id_expression>(
                    std::make_shared<term_expression>(
                        id
                        )
                    );
            }

            inline auto make_id_expression( expression_ptr const& expr )
                -> std::shared_ptr<id_expression>
            {
                return std::make_shared<id_expression>( expr );
            }
        } // namespace helper


        //
        RILL_AST_BEGIN(
            evaluated_type_expression, expression,
            (( type_id_t, type_id ))
            )
        public:
            evaluated_type_expression( type_id_t const& tid )
                : type_id( tid )
            {}

        RILL_AST_END


        /*
        RILL_AST_BEGIN(
            test_while_statement, statement,
            (( expression_ptr, conditional_ ))
            (( block_statement_ptr, body_statement_ ))
            )
        public:
            test_while_statement(
                expression_ptr const& cond,
                block_statement_ptr const& body_statement
                )
                : conditional_( cond )
                , body_statement_( body_statement )
            {}
        RILL_AST_END



        RILL_AST_BEGIN(
            test_if_statement, statement,
            (( expression_ptr, conditional_ ))
            (( block_statement_ptr, then_statement_ ))
            (( boost::optional<block_statement_ptr>, else_statement_ ))
            )
        public:
            test_if_statement(
                expression_ptr const& cond,
                block_statement_ptr const& then_statement,
                boost::optional<block_statement_ptr> const& else_statement
                )
                : conditional_( cond )
                , then_statement_( then_statement )
                , else_statement_( else_statement )
            {}
        RILL_AST_END
        */

    } // namespace ast
} // namespace rill
