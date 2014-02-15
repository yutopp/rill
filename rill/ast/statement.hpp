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

#include "ast_base.hpp"
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
            RILL_MAKE_AST_BASE( statement )

        public:
            virtual ~statement()
            {};
        };


        struct statements RILL_CXX11_FINAL
            : public statement
        {
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

            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                statements,
                (( statement_list, statement_list_,
                   for( auto const& s : statement_list_ )
                       cloned->statement_list_.push_back( clone_ast( s ) );
                    ))
                )
        };



        struct block_statement RILL_CXX11_FINAL
            : public statement
        {
        public:
            block_statement( statements_ptr const& ss )
                : statements_( ss )
            {}

            explicit block_statement( statement_ptr const& s )
                : statements_( std::make_shared<statements>( s ) )
            {}

            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                block_statement,
                (( statements_ptr, statements_ ))
                )
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


        typedef std::vector<variable_declaration_unit> variable_declaration_unit_container_t;


        struct variable_declaration
        {
            attribute::quality_kind quality;        // Ex. val | ref | ...
            variable_declaration_unit decl_unit;
        };


        typedef std::vector<variable_declaration> parameter_list;







        struct expression_statement RILL_CXX11_FINAL
            : public statement
        {
        public:
            expression_statement( expression_ptr const& expr )
                : expression_( expr )
            {}

            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                expression_statement,
                (( expression_ptr, expression_ ))
                )
        };


        struct empty_statement RILL_CXX11_FINAL
            : public statement
        {
            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                empty_statement
                )
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST_INTERFACE(
                can_be_template_statement,
                (( identifier_value_ptr, identifier_ ))
                (( bool, is_templated_, cloned->is_templated_ = is_templated_; ))
                )
        };




        // inner statement will be templated
        struct template_statement RILL_CXX11_FINAL
            : public statement
        {
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

            auto clone_inner_node()
                -> can_be_template_statement_ptr
            {
                auto const& cloned
                    = clone_ast<can_be_template_statement_ptr>( inner_ );

                cloned_inners_.push_back( cloned );

                return cloned;
            }


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                template_statement,
                (( parameter_list, parameter_list_, cloned->parameter_list_ = parameter_list_; ))
                (( can_be_template_statement_ptr, inner_ ))
                )

        private:
            std::vector<can_be_template_statement_ptr> cloned_inners_;
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED_INTERFACE(
                extern_statement_base, can_be_template_statement
                )
        };


        struct extern_function_declaration_statement RILL_CXX11_FINAL
            : public extern_statement_base
        {
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                extern_function_declaration_statement, extern_statement_base,
                (( parameter_list, parameter_list_, cloned->parameter_list_ = parameter_list_; ))
                (( type_expression_ptr, return_type_ ))
                (( native_string_t, extern_symbol_name_, cloned->extern_symbol_name_ = extern_symbol_name_; ))
                )
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
            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED_INTERFACE(
                function_definition_statement_base, can_be_template_statement,
                (( statement_ptr, inner_ ))
                )
        };



        struct function_definition_statement RILL_CXX11_FINAL
            : public function_definition_statement_base
        {
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

 

            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                function_definition_statement, function_definition_statement_base,
                (( parameter_list, parameter_list_, cloned->parameter_list_ = parameter_list_; ))
                (( boost::optional<type_expression_ptr>, return_type_,
                   if ( return_type_ )
                       cloned->return_type_
                           = clone_ast<type_expression_ptr>( *return_type_ );
                    ))
                )
        };


        struct intrinsic_function_definition_statement RILL_CXX11_FINAL
            : public function_definition_statement_base
        {
        public:
            intrinsic_function_definition_statement(
                identifier_value_ptr const& function_name,
                statement_ptr const& inner
                )
                : function_definition_statement_base( function_name, inner )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                intrinsic_function_definition_statement, function_definition_statement_base
                )
        };




        struct class_function_definition_statement RILL_CXX11_FINAL
            : public function_definition_statement_base
        {
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                class_function_definition_statement, can_be_template_statement,
                (( parameter_list, parameter_list_, cloned->parameter_list_ = parameter_list_; ))
                (( boost::optional<type_expression_ptr>, return_type_,
                   if ( return_type_ )
                       cloned->return_type_
                           = clone_ast(
                               std::static_pointer_cast<type_expression_ptr::element_type>( *return_type_ )
                               );
                    ))
                )
        };





        struct class_definition_statement RILL_CXX11_FINAL
            : public can_be_template_statement
        {
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
                , constructor_parameter_list_( constructor_parameter_list ? *constructor_parameter_list : parameter_list() )
                , inner_( inner )
            {}

        public:
            auto get_constructor_parameter_list() const
                -> parameter_list
            {
                return constructor_parameter_list_;
            }


            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                class_definition_statement, can_be_template_statement,
                (( parameter_list, constructor_parameter_list_,
                   cloned->constructor_parameter_list_ = constructor_parameter_list_; ))
                (( statement_ptr, inner_ ))
                )
        };




        struct test_while_statement RILL_CXX11_FINAL
            : public statement
        {
        public:
            test_while_statement(
                expression_ptr const& cond,
                block_statement_ptr const& body_statement
                )
                : conditional_( cond )
                , body_statement_( body_statement )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                test_while_statement,
                (( expression_ptr, conditional_ ))
                (( block_statement_ptr, body_statement_ ))
                )
        };



        struct test_if_statement RILL_CXX11_FINAL
            : public statement
        {
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                test_if_statement,
                (( expression_ptr, conditional_ ))
                (( block_statement_ptr, then_statement_ ))
                (( boost::optional<block_statement_ptr>, else_statement_,
                   if ( else_statement_ )
                       cloned->else_statement_
                           = clone_ast(
                               std::static_pointer_cast<block_statement_ptr::element_type>( *else_statement_ )
                               );
                    ))
                )
        };

 





        struct variable_declaration_statement RILL_CXX11_FINAL
            : public statement
        {
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                variable_declaration_statement,
                (( variable_declaration, declaration_, cloned->declaration_ = declaration_; ))
                )
        };



        struct class_variable_declaration_statement RILL_CXX11_FINAL
            : public statement
        {
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

 
            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                class_variable_declaration_statement,
                (( variable_declaration, declaration_, cloned->declaration_ = declaration_; ))
                )
        };





        struct return_statement RILL_CXX11_FINAL
            : public statement
        {
        public:
            return_statement( expression_ptr const& expr )
                : expression_( expr )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                return_statement,
                (( expression_ptr, expression_ ))
                )
        };


        // pre
        struct jit_statement RILL_CXX11_FINAL
            : public statement
        {
        public:
            jit_statement( expression_ptr const& expr )
                : expression_( expr )
            {}


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                jit_statement,
                (( expression_ptr, expression_ ))
                )
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

