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

#include <boost/optional.hpp>

#include "../environment/environment_fwd.hpp"

#include "detail/tree_visitor_base.hpp"
#include "detail/dispatch_assets.hpp"

#include "../attribute/attribute.hpp"

#include "statement_fwd.hpp"

#include "value.hpp"
#include "expression.hpp"


namespace rill
{
    namespace ast
    {
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        //
        // statements
        //
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------

        // 
        struct statement
            : public ast_base
        {
        public:
            RILL_AST_ADAPT_VISITOR_VIRTUAL( statement )

        public:
            virtual ~statement()
            {};
        };


        struct statements RILL_CXX11_FINAL
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( statements )

        public:
            statements( statement_list const& s )
                : statement_list_( s )
            {}

            statements( statement_ptr const& s )
                : statement_list_( 1, s ) // initialize with one element
            {}
/*
            statements( statement_list&& s )
                : s_( s )
            {}
*/
        public:
            statement_list const statement_list_;
        };



        struct block_statement RILL_CXX11_FINAL
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( block_statement )
            
        public:
            block_statement( statements_ptr const& ss )
                : statements_( ss )
            {}

            explicit block_statement( statement_ptr const& s )
                : statements_( std::make_shared<statements>( s ) )
            {}

        public:
            statements_ptr const statements_;
        };



        //
        struct value_initializer_unit
        {
            expression_ptr initializer;
            type_expression_ptr type;
        };


        struct variable_declaration_unit
        {
            identifier_value_base_ptr name;
            value_initializer_unit init_unit;
        };


        typedef std::vector<variable_declaration_unit> variable_declaration_unit_list;


        struct variable_declaration
        {
            attribute::quality_kind quality;        // Ex. val | ref | ...
            variable_declaration_unit decl_unit;
        };


        typedef std::vector<variable_declaration> parameter_list;







        struct expression_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( expression_statement )

        public:
            expression_statement( expression_ptr const& expr )
                : expression_( expr )
            {}

        public:
            expression_ptr const expression_;
        };


        struct empty_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( empty_statement )

        public:
            empty_statement()
            {}
        };



        struct can_be_template_statement
            : public statement
        {
        public:
            can_be_template_statement( identifier_value_ptr const& symbol_name )
                : identifier_( symbol_name )
                , is_templated_( false )
            {}

            virtual ~can_be_template_statement()
            {}

        public:
            auto get_identifier() const
                -> identifier_value_base_ptr
            {
                return identifier_;
            }
            
            void mark_as_template()
            {
                is_templated_ = true;
            }

            void mark_as_nontemplate()
            {
                is_templated_ = false;
            }

            auto is_templated() const
                -> bool
            {
                return is_templated_;
            }
               
        private:
            identifier_value_ptr identifier_;
            bool is_templated_;
        };




        // inner statement will be templated
        struct template_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( template_statement )

        public:
            template_statement(
                parameter_list const& parameter_list,
                can_be_template_statement_ptr const& inner
                )
                : parameter_list_( parameter_list )
                , inner_( inner )
            {}

        public:
            auto get_identifier() const
                -> identifier_value_base_ptr
            {
                return get_inner_statement()->get_identifier();
            }    

            //
            auto get_inner_statement() const
                -> can_be_template_statement_ptr
            {
                return inner_;
            }

            auto get_parameter_list() const
                -> parameter_list const&
            {
                return parameter_list_;
            }

        public:
            parameter_list parameter_list_;
            can_be_template_statement_ptr const inner_;
        };





        struct extern_statement_base
            : public can_be_template_statement
        {
        public:
            extern_statement_base( identifier_value_ptr const& symbol_name )
                : can_be_template_statement( symbol_name )
            {}

            virtual ~extern_statement_base()
            {}
        };


        struct extern_function_declaration_statement
            : public extern_statement_base
        {
        public:
            RILL_AST_ADAPT_VISITOR( extern_function_declaration_statement )

        public:
            extern_function_declaration_statement(
                identifier_value_ptr const& symbol_name,
                parameter_list const& parameter_list,
                type_expression_ptr const& return_type,
                native_string_t const& extern_symbol_name
                )
                : extern_statement_base( symbol_name )
                , parameter_list_( parameter_list )
                , return_type_( return_type )
                , extern_symbol_name_( extern_symbol_name )
            {}

        public:
            auto get_parameter_list() const
                -> parameter_list
            {
                return parameter_list_;
            }

            auto get_extern_symbol_name() const
                -> native_string_t
            {
                return extern_symbol_name_;
            }

        public:
            parameter_list parameter_list_;
            type_expression_ptr return_type_;

            native_string_t extern_symbol_name_;
        };






        struct function_definition_statement_base
            : public can_be_template_statement
        {
        public:
            function_definition_statement_base(
                identifier_value_ptr const& symbol_name,
                statement_ptr const& inner
                )
                : can_be_template_statement( symbol_name )
                , inner_( inner )
            {}

            virtual ~function_definition_statement_base()
            {}

        public:


        public:
            statement_ptr const inner_;
        };



        struct function_definition_statement
            : public function_definition_statement_base
        {
        public:
            RILL_AST_ADAPT_VISITOR( function_definition_statement )

        public:
            function_definition_statement(
                identifier_value_ptr const& symbol_name,
                parameter_list const& parameter_list,
                boost::optional<type_expression_ptr> const& return_type,
                statement_ptr const& inner
                )
                : function_definition_statement_base( symbol_name, inner )
                , parameter_list_( parameter_list )
                , return_type_( return_type )
            {}

        public:
            auto get_parameter_list() const
                -> parameter_list
            {
                return parameter_list_;
            }

        public:
            parameter_list parameter_list_;
            boost::optional<type_expression_ptr> return_type_;
        };


        struct intrinsic_function_definition_statement
            : public function_definition_statement_base
        {
        public:
            RILL_AST_ADAPT_VISITOR( intrinsic_function_definition_statement )

        public:
            intrinsic_function_definition_statement(
                identifier_value_ptr const& function_name,
                statement_ptr const& inner
                )
                : function_definition_statement_base( function_name, inner )
            {}
        };




        struct class_function_definition_statement
            : public function_definition_statement_base
        {
        public:
            RILL_AST_ADAPT_VISITOR( class_function_definition_statement )

        public:
            class_function_definition_statement(
                identifier_value_ptr const& function_name,
                parameter_list const& parameter_list,
                boost::optional<type_expression_ptr> const& return_type,
                statement_ptr const& inner
                )
                : function_definition_statement_base( function_name, inner )
                , parameter_list_( parameter_list )
                , return_type_( return_type )
            {}

        public:
            auto get_parameter_list() const
                -> parameter_list
            {
                return parameter_list_;
            }

        public:
            parameter_list parameter_list_;
            boost::optional<type_expression_ptr> return_type_;
        };





        struct class_definition_statement
            : public can_be_template_statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( class_definition_statement )

        public:
            class_definition_statement(
                identifier_value_ptr const& identifier
                )
                : can_be_template_statement( identifier )
            {}

            class_definition_statement(
                identifier_value_ptr const& identifier,
                boost::optional<parameter_list> const& constructor_parameter_list,
                statement_ptr const& inner
                )
                : can_be_template_statement( identifier )
                , constructor_parameter_list_( constructor_parameter_list ? std::move( *constructor_parameter_list ) : parameter_list() )
                , inner_( inner )
            {}

        public:
            auto get_constructor_parameter_list() const
                -> parameter_list
            {
                return constructor_parameter_list_;
            }

        public:
            parameter_list const constructor_parameter_list_;
            statement_ptr const inner_;
        };




        struct test_while_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( test_while_statement )

        public:
            test_while_statement(
                expression_ptr const& cond,
                block_statement_ptr const& body_statement
                )
                : conditional_( cond )
                , body_statement_( body_statement )
            {}

        public:
            expression_ptr const conditional_;
            block_statement_ptr const body_statement_;
        };



        struct test_if_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( test_if_statement )

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

        public:
            expression_ptr const conditional_;
            block_statement_ptr const then_statement_;
            boost::optional<block_statement_ptr> else_statement_;
        };

 





        struct variable_declaration_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( variable_declaration_statement )

        public:
            variable_declaration_statement( variable_declaration const& decl )
                : declaration_( decl )
            {}

        public:
            auto get_identifier() const
                -> identifier_value_base_ptr
            {
                return declaration_.decl_unit.name;
            }

        public:
            variable_declaration const declaration_;
        };



        struct class_variable_declaration_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( class_variable_declaration_statement )

        public:
            class_variable_declaration_statement( variable_declaration const& decl )
                : declaration_( decl )
            {}

        public:
            auto get_identifier() const
                -> identifier_value_base_ptr
            {
                return declaration_.decl_unit.name;
            }

        public:
            variable_declaration const declaration_;
        };





        struct return_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( return_statement )

        public:
            return_statement( expression_ptr const& expr )
                : expression_( expr )
            {}

        public:
            expression_ptr const expression_;
        };


        struct jit_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( jit_statement )

        public:
            jit_statement( expression_ptr const& expr )
                : expression_( expr )
            {}

        public:
            expression_ptr const expression_;
        };






        // make native
        inline auto make_native_class( identifier_value_ptr const& class_identifier )
            -> class_definition_statement_ptr
        {
            return std::make_shared<class_definition_statement>( class_identifier );
        }







    } // namespace ast
} // namespace rill


BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::value_initializer_unit,
    (rill::ast::expression_ptr,             initializer)
    (rill::ast::type_expression_ptr,        type)
    )

BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::variable_declaration_unit,
    (rill::ast::identifier_value_base_ptr,  name)
    (rill::ast::value_initializer_unit,     init_unit)
    )

BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::variable_declaration,
    (rill::attribute::quality_kind,         quality)
    (rill::ast::variable_declaration_unit,  decl_unit)
    )

