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

#include <boost/fusion/include/adapt_struct.hpp> /* for boost spirit perser */

#include "../environment/environment_fwd.hpp"

#include "detail/tree_visitor_base.hpp"
#include "detail/dispatch_assets.hpp"

#include "value_fwd.hpp"


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
        enum struct value_spec
        {
            constatnt = 0
        };



        typedef std::string     native_string_t;



        struct value
        {
        public:
            RILL_AST_ADAPT_VISITOR( value )

        public:
            virtual ~value() {}

        public:
            virtual bool is_intrinsic() const
            {
                return false;
            }

            virtual bool is_system() const
            {
                return false;
            }
        };


        namespace intrinsic
        {
            struct value_base
                : public value
            {
            public:
                virtual ~value_base() {}

            public:
                bool is_system() const RILL_CXX11_OVERRIDE RILL_CXX11_FINAL
                {
                    return true;
                }

                virtual auto get_native_type_name_string() const -> native_string_t =0;
            };

            //
            // may be used for symbol literal
            //
            struct symbol_value
                : public value_base
            {
            public:
                explicit symbol_value( native_string_t const& name )
                    : value_( name )
                {}

            public:
                auto get_native_type_name_string() const -> native_string_t RILL_CXX11_OVERRIDE
                {
                    return "symbol";
                }

                auto get_native_string() const
                    -> native_string_t const&
                {
                    return value_;
                }

            private:
                native_string_t value_;
            };

            inline auto make_symbol( native_string_t const& native_symbol_name )
                -> symbol_value_ptr
            {
                return std::make_shared<symbol_value>( native_symbol_name );
            }










            struct single_identifier_value_base
                : value_base
            {
            public:
                virtual ~single_identifier_value_base() {}

            public:
                virtual bool is_template() const =0;

                virtual auto get_base_symbol() const
                    -> symbol_value_ptr =0;

                virtual auto template_argument() const
                    -> template_argument_list_ptr =0;
            };



            // 
            struct identifier_value RILL_CXX11_FINAL
                : public value_base
            {
            public:
                explicit identifier_value( std::vector<single_identifier_value_base_ptr> const& nests )
                    : nest_( nests )
                {}

            public:
                auto get_native_type_name_string() const -> native_string_t RILL_CXX11_OVERRIDE
                {
                    return "identifier";
                }

                auto get_last_identifier() const
                    -> single_identifier_value_base_ptr
                {
                    return nest_.back();
                }

                auto nest_size() const
                    -> std::size_t
                {
                    return nest_.size();
                }

            public:
                std::vector<single_identifier_value_base_ptr> const nest_;
            };


            // TODO: add support for namespaces
            inline auto make_identifier( single_identifier_value_base_ptr const& p )
                -> identifier_value_ptr
            {
                std::vector<single_identifier_value_base_ptr> const l( 1, p );
                return std::make_shared<identifier_value>( l );
            }



            struct single_identifier_value RILL_CXX11_FINAL
                : public single_identifier_value_base
            {
            public:
                single_identifier_value( native_string_t const& single_identifier_string )
                    : base_name_( make_symbol( single_identifier_string ) )
                {}

                single_identifier_value( symbol_value_ptr const& single_identifier_symbol )
                    : base_name_( single_identifier_symbol )
                {}

            public:
                auto get_native_type_name_string() const -> native_string_t RILL_CXX11_OVERRIDE
                {
                    return "single_id"; // TODO: change name
                }

                bool is_template() const RILL_CXX11_OVERRIDE
                {
                    return false;
                }

                auto get_base_symbol() const
                    -> symbol_value_ptr RILL_CXX11_OVERRIDE
                {
                    return base_name_;
                }

                auto template_argument() const
                    -> template_argument_list_ptr RILL_CXX11_OVERRIDE
                {
                    return nullptr;
                }

            private:
                symbol_value_ptr base_name_;
            };

            /*
            //
            struct simple_identifier_value
            : public identifier_value
            {
            public:
            // TODO: support namespace and so on
            simple_identifier_value( native_string_t const& simple_typename );
            public:


            private:
            };
            typedef std::shared_ptr<simple_identifier_value> simple_identifier_value_ptr;
            */

            inline auto make_single_identifier( native_string_t const& simple_typename )
                -> single_identifier_value_ptr
            {
                return std::make_shared<single_identifier_value>( simple_typename );
            }


            inline auto make_binary_operator_identifier(
                native_string_t const& symbol_name
                )
                -> single_identifier_value_ptr
            {
                return make_single_identifier( "%binary%operator_" + symbol_name );
            }
            inline auto make_binary_operator_identifier(
                symbol_value_ptr const& symbol_name
                )
                -> single_identifier_value_ptr
            {
                return make_binary_operator_identifier( symbol_name->get_native_string() );
            }
            // TODO: add overload function that implement template specified operator

            inline auto make_binary_operator_symbol(
                symbol_value_ptr const& symbol_name
                )
                -> symbol_value_ptr
            {
                return make_symbol( "%binary%operator_" + symbol_name->get_native_string() );
            }




            /*

            //
            class template_identifier_value
            : public identifier_value
            {
            public:
            template_identifier_value( symbol_value::native_string_type const& simple_typename );

            public:


            private:
            };*/



            struct int32_value RILL_CXX11_FINAL
                : public value_base
            {
            public:
                int32_value( int const v )
                    : value_( v )
                {}

            public:
                auto get_native_type_name_string() const -> native_string_t RILL_CXX11_OVERRIDE
                {
                    return "int";
                }

                int get_value() const
                {
                    return value_;
                }

            public:
                int const value_;
            };

        }




        //
        struct intrinsic_value
            : public value
        {
        public:
            RILL_AST_ADAPT_VISITOR( intrinsic_value )

        public:
            // specify value's type name
            intrinsic_value( intrinsic::value_base_ptr const& iv )
                : value_( iv )
                , literal_type_name_( std::make_shared<intrinsic::single_identifier_value>( iv->get_native_type_name_string() ) )
            {}

            virtual ~intrinsic_value() {};

        public:
            bool is_intrinsic() const RILL_CXX11_OVERRIDE RILL_CXX11_FINAL
            {
                return true;
            }

        public:
            intrinsic::value_base_ptr value_;
            intrinsic::const_single_identifier_value_ptr literal_type_name_;
        };



        struct variable_value
            : public value
        {
        public:
            RILL_AST_ADAPT_VISITOR( variable_value )

        public:
            variable_value( intrinsic::identifier_value_ptr const& var )
                : variable_name_( var )
            {}

        public:
            intrinsic::identifier_value_ptr variable_name_;
        };

    } // namespace ast
} // namespace rill
