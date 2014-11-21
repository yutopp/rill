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

#include "ast_base.hpp"
#include "value_fwd.hpp"
#include "elements.hpp"


namespace rill
{
    namespace ast
    {
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        //
        // values
        //
        // ----------------------------------------------------------------------
        // ----------------------------------------------------------------------
        typedef std::string     native_string_t;


        RILL_AST_GROUP_BEGIN( value )
        public:
            virtual bool is_intrinsic() const
            {
                return false;
            }

            virtual bool is_system() const
            {
                return false;
            }
        RILL_AST_GROUP_END


        //
        namespace intrinsic
        {
            //
            // Intrinsic Value Base
            //
            RILL_AST_BEGIN(
                value_base, value
                )
            public:
                bool is_system() const RILL_CXX11_FINAL
                {
                    return true;
                }

                virtual auto get_native_typename_string() const
                    -> native_string_t =0;
            RILL_AST_END


            //
            // Symbol Literal
            //
            RILL_AST_BEGIN(
                symbol_value, value_base,
                (( native_string_t, value_ ))
                )
            public:
                explicit symbol_value( native_string_t const& name )
                    : value_( name )
                {}

            public:
                auto get_native_typename_string() const
                    -> native_string_t override final
                {
                    return "symbol";
                }

                auto to_native_string() const
                    -> native_string_t const&
                {
                    return value_;
                }
            RILL_AST_END


            inline auto make_symbol( native_string_t const& native_symbol_name )
                -> symbol_value_ptr
            {
                return std::make_shared<symbol_value>( native_symbol_name );
            }



            //
            // Int32
            //
            RILL_AST_BEGIN(
                int32_value, value_base,
                (( int, value_ ))
                )
            public:
                int32_value( int const v )
                    : value_( v )
                {}

            public:
                virtual auto get_native_typename_string() const -> native_string_t
                {
                    return "int";
                }

                int get_value() const
                {
                    return value_;
                }
            RILL_AST_END

            RILL_AST_BEGIN(
                float_value, value_base,
                (( float, value_ ))
                )
            public:
                float_value( int const v )
                    : value_( v )
                {}

            public:
                virtual auto get_native_typename_string() const -> native_string_t
                {
                    return "float";
                }

                auto get_value() const
                    -> float const&
                {
                    return value_;
                }
            RILL_AST_END

            //
            RILL_AST_BEGIN(
                boolean_value, value_base,
                (( bool, value_ ))
                )
            public:
                boolean_value( bool const v )
                    : value_( v )
                {}

            public:
                virtual auto get_native_typename_string() const -> native_string_t
                {
                    return "bool";
                }

                bool get_value() const
                {
                    return value_;
                }
            RILL_AST_END




            //
            // String Value
            //
            RILL_AST_BEGIN(
                string_value, value_base,
                (( std::string, value_ ))
                )
            public:
                string_value( std::string const& v )
                    : value_( v )
                {}

            public:
                virtual auto get_native_typename_string() const -> native_string_t
                {
                    return "string";
                }

                auto get_value() const
                    -> std::string const&
                {
                    return value_;
                }
            RILL_AST_END



            //
            // Array Value
            //
            RILL_AST_BEGIN(
                array_value, value_base,
                (( expression_list, elements_list_ ))
                )
            public:
                array_value( expression_list const& ex )
                    : elements_list_( ex )
                {}

            public:
                virtual auto get_native_typename_string() const
                    -> native_string_t
                {
                    return "array";
                }
            RILL_AST_END

        } // namespace intrinsic


        //
        //
        //
        RILL_AST_BEGIN(
            identifier_value_base, value,
            (( intrinsic::symbol_value_ptr, name_symbol_ ))
            (( bool, started_from_root_))
            )
        public:
            identifier_value_base(
                intrinsic::symbol_value_ptr const& name,
                bool const started_from_root = false
                )
                : name_symbol_( name )
                , started_from_root_( started_from_root )
            {
                rill_dout << "IDENTIFIER INSTNCED: " << this << " / " << name->to_native_string() << std::endl;
            }

        public:
            virtual auto get_native_typename_string() const
                -> native_string_t =0;

            virtual auto is_template() const
                -> bool =0;
            virtual auto template_argument() const
                -> expression_list const& =0;

            auto get_inner_symbol() const
                -> intrinsic::symbol_value_ptr
                {
                    return name_symbol_;
                }

            auto is_started_from_root() const
                -> bool
                {
                    return started_from_root_;
                }
        RILL_AST_END


        //
        //
        //
        RILL_AST_BEGIN(
            identifier_value, identifier_value_base,
            )
        public:
            identifier_value(
                native_string_t const& name,
                bool const started_from_root = false
                )
                : identifier_value( intrinsic::make_symbol( name ), started_from_root )
            {}

            identifier_value(
                intrinsic::symbol_value_ptr const& name,
                bool const started_from_root = false
                )
                : identifier_value_base( name, started_from_root )
            {}

        public:
            virtual auto get_native_typename_string() const
                -> native_string_t
            {
                return "identifier"; // TODO: change name
            }

            virtual auto is_template() const
                -> bool
            {
                return false;
            }

            virtual auto template_argument() const
                -> expression_list const&
            {
                // TODO: fix
                expression_list static a;
                return a;
            }
        RILL_AST_END


        inline auto make_identifier( native_string_t const& simple_typename )
            -> identifier_value_ptr
        {
            return std::make_shared<identifier_value>( simple_typename );
        }

        inline auto make_binary_operator_identifier(
            native_string_t const& symbol_name
            )
            -> identifier_value_ptr
        {
            return make_identifier( "%binary%operator_" + symbol_name );
        }

        inline auto make_unary_prefix_operator_identifier(
            native_string_t const& symbol_name
            )
            -> identifier_value_ptr
        {
            return make_identifier( "%unary%prefix%operator_" + symbol_name );
        }

        inline auto make_unary_postfix_operator_identifier(
            native_string_t const& symbol_name
            )
            -> identifier_value_ptr
        {
            return make_identifier( "%unary%postfix%operator_" + symbol_name );
        }


        //
        //
        //
        RILL_AST_BEGIN(
            template_instance_value, identifier_value_base,
            (( expression_list, template_args_ ))
            )
        public:
            template_instance_value(
                native_string_t const& name,
                expression_list const& arguments,
                bool const started_from_root = false
                )
                : template_instance_value( intrinsic::make_symbol( name ), arguments, started_from_root )
            {}

            // TODO: implement
            template_instance_value(
                intrinsic::symbol_value_ptr const& name,
                expression_list const& arguments,
                bool const started_from_root = false
                )
                : identifier_value_base( name, started_from_root )
                , template_args_( arguments )
            {}

        public:
            virtual auto get_native_typename_string() const
                -> native_string_t
            {
                return "template_identifier"; // TODO: change name
            }

            virtual auto is_template() const
                -> bool
            {
                return true;
            }

            virtual auto template_argument() const
                -> expression_list const&
            {
                return template_args_;
            }
        RILL_AST_END

    } // namespace ast
} // namespace rill
