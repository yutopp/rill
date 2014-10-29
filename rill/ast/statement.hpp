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
#include "../type/attribute.hpp"

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
        // statements
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        RILL_AST_GROUP_BEGIN( statement )
        RILL_AST_GROUP_END


        //
        RILL_AST_BEGIN(
            statements, statement,
            (( element::statement_list, statements_ ))
            )
        public:
            statements( element::statement_list const& s )
                : statements_( s )
            {}

            statements( statement_ptr const& s )
                : statements_( 1, s ) // initialize with one element
            {}
        RILL_AST_END



        RILL_AST_BEGIN(
            block_statement, statement,
            (( statements_ptr, statements_ ))
            )
        public:
            block_statement( statements_ptr const& ss )
                : statements_( ss )
            {}

            explicit block_statement( statement_ptr const& s )
                : statements_( std::make_shared<statements>( s ) )
            {}
        RILL_AST_END



        //
        struct value_initializer_unit
        {
            value_initializer_unit() = default;

            value_initializer_unit(
                expression_ptr const& ep
                )
                : initializer( ep )
                , type( nullptr )
            {}

            value_initializer_unit(
                id_expression_ptr const& tp,
                boost::optional<expression_ptr> const& ep
                )
                : initializer( ep != boost::none ? *ep : nullptr )
                , type( tp )
            {}

            value_initializer_unit( value_initializer_unit const& rhs )
                : initializer( clone( rhs.initializer ) )
                , type( clone( rhs.type ) )
            {}
            value_initializer_unit& operator=( value_initializer_unit const& ) =default;

            value_initializer_unit( value_initializer_unit&& rhs )
                : initializer( std::move( rhs.initializer ) )
                , type( std::move( rhs.type ) )
            {}
            value_initializer_unit& operator=( value_initializer_unit&& ) =default;

            expression_ptr initializer;
            id_expression_ptr type;
        };


        struct variable_declaration_unit
        {
            variable_declaration_unit() = default;

            variable_declaration_unit(
                identifier_value_base_ptr const& n,
                value_initializer_unit const& i
                )
                : name( n )
                , init_unit( i )
            {}

            template<typename T>
            variable_declaration_unit(
                boost::optional<T> const& n,
                value_initializer_unit const& i
                )
                : variable_declaration_unit(
                    n != boost::none ? *n : nullptr,
                    i
                    )
            {}

            variable_declaration_unit( variable_declaration_unit const& rhs )
                : name( clone( rhs.name ) )
                , init_unit( rhs.init_unit )
            {}
            variable_declaration_unit& operator=( variable_declaration_unit const& ) =default;

            variable_declaration_unit( variable_declaration_unit&& rhs )
                : name( std::move( rhs.name ) )
                , init_unit( std::move( rhs.init_unit ) )
            {}
            variable_declaration_unit& operator=( variable_declaration_unit&& ) =default;

            identifier_value_base_ptr name;
            value_initializer_unit init_unit;
        };


        typedef std::vector<variable_declaration_unit> variable_declaration_unit_container_t;


        struct variable_declaration
        {
            attribute::holder_kind quality;        // Ex. val | ref | ...
            variable_declaration_unit decl_unit;
        };


        typedef std::vector<variable_declaration> parameter_list;







        RILL_AST_BEGIN(
            expression_statement, statement,
            (( expression_ptr, expression_ ))
            )
        public:
            expression_statement( expression_ptr const& expr )
                : expression_( expr )
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            empty_statement, statement
            )
        RILL_AST_END



        RILL_AST_BEGIN(
            can_be_template_statement, statement,
            (( identifier_value_ptr, identifier_ ))
            (( bool, is_templated_ ))
            )
        public:
            can_be_template_statement( identifier_value_ptr const& id )
                : identifier_( id )
                , is_templated_( false )
            {}

        public:
            auto get_identifier() const
                -> identifier_value_ptr
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
        RILL_AST_END




        // inner statement will be templated
        RILL_AST_BEGIN(
            template_statement, statement,
            (( parameter_list, parameter_list_ ))
            (( can_be_template_statement_ptr, inner_ ))
            )
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
                -> identifier_value_ptr
            {
                return get_inner_statement()->get_identifier();
            }

            //
            auto get_inner_statement() const
                -> can_be_template_statement_ptr const&
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
                auto const& cloned = clone( inner_ );

                cloned_inners_.push_back( cloned );

                return cloned;
            }

        private:
            std::vector<can_be_template_statement_ptr> cloned_inners_;
        RILL_AST_END





        RILL_AST_BEGIN(
            extern_statement_base, can_be_template_statement
            )
        public:
            extern_statement_base( identifier_value_ptr const& id )
                : can_be_template_statement( id )
            {}
        RILL_AST_END


        RILL_AST_BEGIN(
            extern_function_declaration_statement, extern_statement_base,
            (( parameter_list, parameter_list_ ))
            (( id_expression_ptr, return_type_ ))
            (( native_string_t, extern_symbol_name_ ))
            )
        public:
            extern_function_declaration_statement(
                identifier_value_ptr const& id,
                parameter_list const& parameter_list,
                id_expression_ptr const& return_type,
                native_string_t const& extern_symbol_name
                )
                : extern_statement_base( id )
                , parameter_list_( parameter_list )
                , return_type_( return_type )
                , extern_symbol_name_( extern_symbol_name )
            {}

        public:
            auto get_parameter_list() const
                -> parameter_list const&
            {
                return parameter_list_;
            }

            auto get_extern_symbol_name() const
                -> native_string_t const&
            {
                return extern_symbol_name_;
            }
        RILL_AST_END






        RILL_AST_BEGIN(
            function_definition_statement_base, can_be_template_statement,
            (( statement_ptr, inner_ ))
            )
        public:
            function_definition_statement_base(
                identifier_value_ptr const& id,
                statement_ptr const& inner
                )
                : can_be_template_statement( id )
                , inner_( inner )
            {}
        RILL_AST_END



        RILL_AST_BEGIN(
            function_definition_statement, function_definition_statement_base,
            (( parameter_list, parameter_list_ ))
            (( id_expression_ptr, return_type_ ))
            )
        public:
            function_definition_statement(
                identifier_value_ptr const& id,
                parameter_list const& parameter_list,
                boost::optional<id_expression_ptr> const& return_type,
                statement_ptr const& inner
                )
                : function_definition_statement_base( id, inner )
                , parameter_list_( parameter_list )
                , return_type_( return_type != boost::none ? *return_type : nullptr )
            {}

        public:
            auto get_parameter_list() const
                -> parameter_list const&
            {
                return parameter_list_;
            }
        RILL_AST_END


        RILL_AST_BEGIN(
            intrinsic_function_definition_statement, function_definition_statement_base
            )
        public:
            intrinsic_function_definition_statement(
                identifier_value_ptr const& id,
                statement_ptr const& inner
                )
                : function_definition_statement_base( id, inner )
            {}
        RILL_AST_END




        RILL_AST_BEGIN(
            class_function_definition_statement, function_definition_statement_base,
            (( parameter_list, parameter_list_ ))
            (( id_expression_ptr, return_type_ ))
            )
        public:
            class_function_definition_statement(
                identifier_value_ptr const& id,
                parameter_list const& parameter_list,
                boost::optional<id_expression_ptr> const& return_type,
                statement_ptr const& inner
                )
                : function_definition_statement_base( id, inner )
                , parameter_list_( parameter_list )
                , return_type_( return_type != boost::none ? *return_type : nullptr )
            {}

        public:
            auto get_parameter_list() const
                -> parameter_list const&
            {
                return parameter_list_;
            }
        RILL_AST_END





        RILL_AST_BEGIN(
            class_definition_statement, can_be_template_statement,
            (( statement_ptr, inner_ ))
            )
        public:
            class_definition_statement(
                identifier_value_ptr const& id
                )
                : can_be_template_statement( id )
            {}

            class_definition_statement(
                identifier_value_ptr const& id,
                statement_ptr const& inner
                )
                : can_be_template_statement( id )
                , inner_( inner )
            {}
        RILL_AST_END









        RILL_AST_BEGIN(
            variable_declaration_statement, statement,
            (( variable_declaration, declaration_ ))
            )
        public:
            variable_declaration_statement( variable_declaration const& decl )
                : declaration_( decl )
            {}

        public:
            auto get_identifier() const
                -> identifier_value_base_ptr const&
            {
                return declaration_.decl_unit.name;
            }
        RILL_AST_END



        RILL_AST_BEGIN(
            class_variable_declaration_statement, statement,
            (( variable_declaration, declaration_ ))
            )
        public:
            class_variable_declaration_statement( variable_declaration&& decl )
                : declaration_( std::move( decl ) )
            {}

        public:
            auto get_identifier() const
                -> identifier_value_base_ptr const&
            {
                return declaration_.decl_unit.name;
            }
        RILL_AST_END





        RILL_AST_BEGIN(
            return_statement, statement,
            (( expression_ptr, expression_ ))
            )
        public:
            return_statement( expression_ptr const& expr )
                : expression_( expr )
            {}
        RILL_AST_END



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



        // make native
        inline auto make_native_class(
            identifier_value_ptr const& class_identifier
            )
            -> class_definition_statement_ptr
        {
            return std::make_shared<class_definition_statement>(
                class_identifier
                );
        }







    } // namespace ast
} // namespace rill
/*
BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::variable_declaration_unit,
    (rill::ast::identifier_value_base_ptr,  name)
    (rill::ast::value_initializer_unit,     init_unit)
    )
*/
