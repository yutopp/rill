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

#include "../environment_fwd.hpp"
#include "../tree_visitor_base.hpp"

#include "detail/macros.hpp"

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
        {
        public:
            RILL_AST_ADAPT_VISITOR( statement )

        public:
            virtual ~statement()
            {};
        };



        //
        struct value_initializer_unit
        {
            expression_ptr initializer;
            type_expression_ptr type;
        };

        struct variable_declaration_unit
        {
            intrinsic::identifier_value_ptr name;
            value_initializer_unit init_unit;
        };

        struct variable_declaration
        {
            // TODO: add declaration type information(Ex. val OR ref... and so on
            
            variable_declaration_unit decl_unit;
        };

        // TODO: change to declaration statement
        typedef std::vector<variable_declaration> parameter_list;

        /*
        // TODO: change to declaration statement
        inline auto make_variable_declaration(
            intrinsic::identifier_value_ptr const& name = nullptr,
            expression_ptr const& initializer = nullptr,
            type_expression_ptr const& type = nullptr
            )
            -> variable_declaration_unit
        {
            value_initializer_unit vi = { initializer, type };
            variable_declaration_unit du = { name, vi };

            return du;
        }


        // TODO: change to declaration statement
        inline auto make_variable_declaration(
            intrinsic::identifier_value_ptr const& name = nullptr,
            expression_ptr const& initializer = nullptr,
            type_expression_ptr const& type = nullptr
            )
            -> variable_declaration_unit
        {
            value_initializer_unit vi = { initializer, type };
            variable_declaration_unit du = { name, vi };

            return du;
        }
        */
/*
        // test imprementation
        inline auto make_parameter_list(
            parameter_pair const& pp
            )
            -> parameter_list
        {
            parameter_list pl;
            pl.push_back( pp ); // test code

            return pl;
        }
        */


        //
        template<typename Target>
        struct template_statement
            : statement
        {
        public:
//            RILL_AST_ADAPT_VISITOR( template_statement )
        };




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






        struct function_definition_statement_base
            : public statement
        {
//            ADAPT_STATEMENT_VISITOR( function_definition_statement_base )

        public:
            function_definition_statement_base(
                intrinsic::identifier_value_ptr const& symbol_name,
                parameter_list const& parameter_list,
                boost::optional<intrinsic::identifier_value_ptr> const& return_type
                )
                : identifier_( symbol_name )
                , parameter_list_( parameter_list )
                , return_type_( return_type )
            {}

            virtual ~function_definition_statement_base()
            {}

        public:
            auto get_identifier() const
                -> intrinsic::identifier_value_ptr
            {
                return identifier_;
            }

            auto get_parameter_list() const
                -> parameter_list
            {
                return parameter_list_;
            }

        public:
            intrinsic::identifier_value_ptr identifier_;
            parameter_list parameter_list_;
            boost::optional<intrinsic::identifier_value_ptr> return_type_;
        };



        struct function_definition_statement
            : public function_definition_statement_base
        {
        public:
            RILL_AST_ADAPT_VISITOR( function_definition_statement )

        public:
            function_definition_statement(
                intrinsic::identifier_value_ptr const& symbol_name,
                parameter_list const& parameter_list,
                boost::optional<intrinsic::identifier_value_ptr> const& return_type,
                statement_list const& statements
                )
                : function_definition_statement_base( symbol_name, parameter_list, return_type )
                , statements_( statements )
            {}

        public:

        public:
            statement_list const statements_;
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

        /*

        #include <functional>
        typedef std::function<value_ptr(std::vector<value_ptr> const&)> native_function_t;

        struct native_function_definition_statement
        : public function_definition_statement_base
        {
        ADAPT_STATEMENT_VISITOR( native_function_definition_statement )

        public:
        native_function_definition_statement(
        intrinsic::identifier_value_ptr const& symbol_name,
        parameter_list const& parameter_list,
        intrinsic::identifier_value_ptr const& return_type,
        native_function_t const& callee
        )
        : function_definition_statement_base( symbol_name, parameter_list, return_type )
        , callee_( callee )
        {}

        public:
        native_function_t const callee_;
        };
        */




        struct class_definition_statement
            : public statement
        {
        public:
            RILL_AST_ADAPT_VISITOR( class_definition_statement )

        public:
            class_definition_statement( intrinsic::identifier_value_ptr const& identifier )
                : identifier_( identifier )
            {}

            //virtual ~class_definition_statement {}

        public:
            //void setup_environment( environment_ptr const& ) const {}

            auto get_identifier() const
                -> intrinsic::identifier_value_ptr
            {
                return identifier_;
            }

        private:
            intrinsic::identifier_value_ptr identifier_;
        };



        // make native
        inline auto make_native_class( intrinsic::identifier_value_ptr const& class_name )
            -> class_definition_statement_ptr
        {
            // TODO: insert assert that checks class_name depth.

            return std::make_shared<class_definition_statement>( class_name );
        }






        struct block_statement
            : public statement
        {
//            ADAPT_STATEMENT_VISITOR( block_statement )

        public:
            block_statement( statement_list const& statements )
                : statements_( statements )
            {}

        public:
            statement_list statements_;
        };

    } // namespace ast
} // namespace rill


BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::value_initializer_unit,
    (rill::ast::expression_ptr,      initializer)
    (rill::ast::type_expression_ptr, type)
    )

BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::variable_declaration_unit,
    (rill::ast::intrinsic::identifier_value_ptr, name)
    (rill::ast::value_initializer_unit,          init_unit)
    )

BOOST_FUSION_ADAPT_STRUCT(
    rill::ast::variable_declaration,
    (rill::ast::variable_declaration_unit,  decl_unit)
    )