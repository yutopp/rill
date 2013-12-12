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

#include "ast_base.hpp"


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
            : public ast_base
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
                bool is_system() const  RILL_CXX11_FINAL
                {
                    return true;
                }

                virtual auto get_native_typename_string() const -> native_string_t =0;
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
                virtual auto get_native_typename_string() const -> native_string_t
                {
                    return "symbol";
                }

                auto to_native_string() const
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


            struct nested_identifier_value RILL_CXX11_FINAL
                : public value_base
            {
            public:
                explicit nested_identifier_value(
                    std::vector<identifier_value_base_ptr> const& ids,
                    bool const started_from_root = false
                    )
                    : ids_( ids )
                    , started_from_root_( started_from_root )
                {}

            public:
                virtual auto get_native_typename_string() const
                    -> native_string_t
                {
                    return "identifier";
                }

                auto at(std::size_t const& index) const
                    -> identifier_value_base_ptr
                {
                    return ids_.at( index );
                }

                auto last() const
                    -> identifier_value_base_ptr
                {
                    return ids_.back();
                }

                auto nest_size() const
                    -> std::size_t
                {
                    return ids_.size();
                }

                auto get_nest_ids() const
                    -> std::vector<identifier_value_base_ptr> const&
                {
                    return ids_;
                }

                auto is_started_from_root() const
                    -> bool
                {
                    return started_from_root_;
                }

            private:
                std::vector<identifier_value_base_ptr> const ids_;
                bool started_from_root_;
            };


            // TODO: add support for namespaces
            inline auto make_nested_identifier( identifier_value_base_ptr const& p )
                -> nested_identifier_value_ptr
            {
                std::vector<identifier_value_base_ptr> const l( 1, p );
                return std::make_shared<nested_identifier_value>( l );
            }



            //
            //
            //
            struct identifier_value_base
                : public value_base
            {
            public:
                identifier_value_base(
                    symbol_value_ptr const& name,
                    bool const started_from_root = false
                    )
                    : name_symbol_( name )
                    , started_from_root_( started_from_root )
                {}

                virtual ~identifier_value_base() {}

            public:
                virtual auto get_native_typename_string() const
                    -> native_string_t =0;

                virtual auto is_template() const
                    -> bool =0;
                virtual auto template_argument() const
                    -> template_argument_list_ptr =0;

                auto get_inner_symbol() const
                    -> symbol_value_ptr
                {
                    return name_symbol_;
                }

                auto is_started_from_root() const
                    -> bool
                {
                    return started_from_root_;
                }

            private:
                symbol_value_ptr const name_symbol_;
                bool const started_from_root_;
            };



            struct identifier_value RILL_CXX11_FINAL
                : public identifier_value_base
            {
            public:
                identifier_value(
                    native_string_t const& name,
                    bool const started_from_root = false
                    )
                    : identifier_value( make_symbol( name ), started_from_root )
                {}

                identifier_value(
                    symbol_value_ptr const& name,
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
                    -> template_argument_list_ptr
                {
                    return nullptr;
                }
            };



            inline auto make_single_identifier( native_string_t const& simple_typename )
                -> identifier_value_ptr
            {
                return std::make_shared<identifier_value>( simple_typename );
            }


            inline auto make_binary_operator_identifier(
                native_string_t const& symbol_name
                )
                -> identifier_value_ptr
            {
                return make_single_identifier( "%binary%operator_" + symbol_name );
            }
            inline auto make_binary_operator_identifier(
                symbol_value_ptr const& symbol_name
                )
                -> identifier_value_ptr
            {
                return make_binary_operator_identifier( symbol_name->to_native_string() );
            }
            // TODO: add overload function that implement template specified operator

            inline auto make_binary_operator_symbol(
                symbol_value_ptr const& symbol_name
                )
                -> symbol_value_ptr
            {
                return make_symbol( "%binary%operator_" + symbol_name->to_native_string() );
            }




            class template_instance_value RILL_CXX11_FINAL
                : public identifier_value_base
            {
            public:
                template_instance_value(
                    native_string_t const& name,
                    bool const started_from_root = false
                    )
                    : template_instance_value( make_symbol( name ), started_from_root )
                {}

                // TODO: implement
                template_instance_value(
                    symbol_value_ptr const& name,
                    bool const started_from_root = false
                    )
                    : identifier_value_base( name, started_from_root )
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
                    -> template_argument_list_ptr
                {
                    return nullptr; // TODO: implement
                }

            private:
            };



            struct int32_value RILL_CXX11_FINAL
                : public value_base
            {
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

            public:
                int const value_;
            };


            struct boolean_value RILL_CXX11_FINAL
                : public value_base
            {
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

            public:
                bool const value_;
            };


            struct string_value RILL_CXX11_FINAL
                : public value_base
            {
            public:
                string_value( std::string const& v )
                    : value_( v )
                {}

            public:
                virtual auto get_native_typename_string() const -> native_string_t
                {
                    return "string";
                }


            public:
                std::string value_;
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
                , literal_type_name_( std::make_shared<intrinsic::identifier_value>( iv->get_native_typename_string() ) )
            {}

            virtual ~intrinsic_value() {};

        public:
            virtual bool is_intrinsic() const RILL_CXX11_FINAL
            {
                return true;
            }

        public:
            intrinsic::value_base_ptr value_;   // TODO: rename(holder)
            intrinsic::const_identifier_value_ptr literal_type_name_;
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
