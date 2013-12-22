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

#include "../ast/detail/tree_visitor_base.hpp"


namespace rill
{
    namespace debug
    {
        class ast_printer RILL_CXX11_FINAL
            : public ast::detail::tree_visitor<ast_printer, void>
        {
        public:
            // statement_list
            RILL_TV_OP_DECL( ast::root )

            RILL_TV_OP_DECL( ast::block_statement )

            // statement
            RILL_TV_OP_DECL( ast::expression_statement )
            RILL_TV_OP_DECL( ast::return_statement )
            RILL_TV_OP_DECL( ast::function_definition_statement )
            RILL_TV_OP_DECL( ast::class_definition_statement )
            RILL_TV_OP_DECL( ast::variable_declaration_statement )
            RILL_TV_OP_DECL( ast::extern_function_declaration_statement )
            RILL_TV_OP_DECL( ast::class_function_definition_statement )
            RILL_TV_OP_DECL( ast::class_variable_declaration_statement )
            RILL_TV_OP_DECL( ast::intrinsic_function_definition_statement )

            RILL_TV_OP_DECL( ast::test_while_statement )
            RILL_TV_OP_DECL( ast::test_if_statement )

            // expression
            RILL_TV_OP_DECL( ast::binary_operator_expression )
            RILL_TV_OP_DECL( ast::element_selector_expression )
            RILL_TV_OP_DECL( ast::call_expression )
            RILL_TV_OP_DECL( ast::intrinsic_function_call_expression )
            RILL_TV_OP_DECL( ast::term_expression )

            // value
            RILL_TV_OP_DECL( ast::nested_identifier_value )
            RILL_TV_OP_DECL( ast::identifier_value )
            //RILL_TV_OP_DECL( ast::template_instance_value )
            RILL_TV_OP_DECL( ast::literal_value )

                RILL_TV_OP_FAIL

        private:
            template<typename T, typename F>
            auto o( T const& name, F call )
                -> void
            {
                std::cout << std::string( indent_ * 4, ' ' ) << " = call: " << name << std::endl;

                ++indent_;
                call( std::string( indent_ * 4, ' ' ) );
                --indent_;
            }
        private:
            std::size_t indent_ = 0;
        };

    } // namespace debug
} // namespace rill

#endif /*RILL_DEBUG_AST_PRINTER_HPP*/
