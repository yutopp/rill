//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_INTERPRETER_RUNNER_HPP
#define RILL_INTERPRETER_RUNNER_HPP

#include "../ast/detail/tree_visitor_base.hpp"
#include "../utility/embedded_function_holder.hpp"

#include "runtime.hpp"


namespace rill
{
    namespace interpreter
    {
        class runner RILL_CXX11_FINAL
            : public ast::detail::tree_visitor_base<environment_base_ptr>
        {
        public:
            runner( context_ptr const&, embedded_function_holder_ptr const& );

        public:
            // statement_list
            RILL_TV_OP_DECL( ast::root )

            // statement
            // virtual void operator()( template_statement const& s, environment_base_ptr const& env ) const =0;

            RILL_TV_OP_DECL( ast::expression_statement )
            RILL_TV_OP_DECL( ast::return_statement )
            RILL_TV_OP_DECL( ast::function_definition_statement )
            // virtual void operator()( native_function_definition_statement const& s, environment_base_ptr const& env ) const =0;

            RILL_TV_OP_DECL( ast::class_definition_statement )

            // expression
            RILL_TV_OP_DECL( ast::binary_operator_expression )
            RILL_TV_OP_DECL( ast::call_expression )
            RILL_TV_OP_DECL( ast::embedded_function_call_expression )
            RILL_TV_OP_DECL( ast::term_expression )

            RILL_TV_OP_DECL( ast::type_identifier_expression )
            RILL_TV_OP_DECL( ast::compiletime_return_type_expression )


            //
            RILL_TV_OP_DECL( ast::intrinsic_value )
            RILL_TV_OP_DECL( ast::variable_value )

        private:
            context_ptr context_;
            embedded_function_holder_ptr action_holder_;
        };

    } // namespace interpreter
} // namespace rill

#endif /*RILL_INTERPRETER_RUNNER_HPP*/
