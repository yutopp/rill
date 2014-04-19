//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_DEBUG_AST_PRINTER_HPP
#define RILL_DEBUG_AST_PRINTER_HPP

#include <iostream>

#include "../ast/visitor.hpp"


namespace rill
{
    namespace debug
    {
        class ast_printer RILL_CXX11_FINAL
            : public ast::readonly_ast_visitor_const<ast_printer, void>
        {
        public:
            // statement
            RILL_VISITOR_READONLY_OP_DECL( ast::statements ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::block_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::expression_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::return_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::function_definition_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::class_definition_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::variable_declaration_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::extern_function_declaration_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::class_function_definition_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::class_variable_declaration_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic_function_definition_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::template_statement ) const;

            RILL_VISITOR_READONLY_OP_DECL( ast::test_while_statement ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::test_if_statement ) const;

            // expression
            RILL_VISITOR_READONLY_OP_DECL( ast::binary_operator_expression ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::element_selector_expression ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::call_expression ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic_function_call_expression ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::term_expression ) const;

            // value
            RILL_VISITOR_READONLY_OP_DECL( ast::nested_identifier_value ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::identifier_value ) const;
            //RILL_VISITOR_READONLY_OP_DECL( ast::template_instance_value ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::int32_value ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::boolean_value ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::string_value ) const;
            RILL_VISITOR_READONLY_OP_DECL( ast::intrinsic::array_value ) const;

            RILL_VISITOR_OP_FAIL

        private:
            template<typename T, typename F>
            auto o( T const& name, F call ) const
                -> void
            {
                std::cout << std::string( indent_ * 4, ' ' ) << " = call: " << name << std::endl;

                ++indent_;
                call( std::string( indent_ * 4, ' ' ) );
                --indent_;
            }

        private:
            mutable std::size_t indent_ = 0;
        };

    } // namespace debug
} // namespace rill

#endif /*RILL_DEBUG_AST_PRINTER_HPP*/
