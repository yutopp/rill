//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP
#define RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP

#include "../ast/detail/tree_visitor_base.hpp"


namespace rill
{
    namespace code_generator
    {
        class llvm_ir_generator RILL_CXX11_FINAL
            : public ast::detail::tree_visitor_base<llvm::Value*>
        {
        public:
            // statement_list
            RILL_TV_OP_DECL( ast::root )

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            RILL_TV_OP_DECL( ast::expression_statement )
            //RILL_TV_OP_DECL( ast::return_statement )
            //RILL_TV_OP_DECL( ast::function_definition_statement )
            // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

            //RILL_TV_OP_DECL( ast::class_definition_statement )

            // expression
            RILL_TV_OP_DECL( ast::binary_operator_expression )
            //RILL_TV_OP_DECL( ast::call_expression )
            //RILL_TV_OP_DECL( ast::embedded_function_call_expression_ptr )
            RILL_TV_OP_DECL( ast::term_expression )

            //
            RILL_TV_OP_DECL( ast::intrinsic_value )
            //RILL_TV_OP_DECL( ast::variable_value )
        };
    } // namespace code_generator
} // namespace rill

#endif /*RILL_CODE_GENERATOR_LLVM_IR_GENERATOR_HPP*/