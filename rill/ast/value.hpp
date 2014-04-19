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

#include "ast_base.hpp"
#include "value_fwd.hpp"
// expression_fwd is required to implement template identifier...
#include "expression_fwd.hpp"


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

        //enum struct value_spec
        //{
        //    constatnt = 0
        //};

        typedef std::string     native_string_t;



        RILL_AST_CORE_BEGIN( value )
        public:
            virtual bool is_intrinsic() const
            {
                return false;
            }

            virtual bool is_system() const
            {
                return false;
            }

        RILL_AST_CORE_END



        //
        namespace intrinsic
        {
            //
            // Intrinsic Value Base
            //
            RILL_AST_INTERFACE_BEGIN( value_base, value )
            public:
                bool is_system() const RILL_CXX11_FINAL
                {
                    return true;
                }

                virtual auto get_native_typename_string() const
                    -> native_string_t =0;

            RILL_AST_INTERFACE_END



            //
            // Symbol Literal
            //
            struct symbol_value RILL_CXX11_FINAL
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


                //////////////////////////////////////////////////
                RILL_MAKE_AST_DERIVED(
                    symbol_value, value_base,
                    (( native_string_t, value_, cloned->value_ = value_; ))
                    )
            };

            inline auto make_symbol( native_string_t const& native_symbol_name )
                -> symbol_value_ptr
            {
                return std::make_shared<symbol_value>( native_symbol_name );
            }



            //
            // Int32
            //
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


                //////////////////////////////////////////////////
                RILL_MAKE_AST_DERIVED(
                    int32_value, value_base,
                    (( int, value_, cloned->value_ = value_; ))
                    )
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


                //////////////////////////////////////////////////
                RILL_MAKE_AST_DERIVED(
                    boolean_value, value_base,
                    (( bool, value_, cloned->value_ = value_; ))
                    )
            };



            //
            // String Value
            //
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

                auto get_value() const
                    -> std::string const&
                {
                    return value_;
                }

                //////////////////////////////////////////////////
                RILL_MAKE_AST_DERIVED(
                    string_value, value_base,
                    (( std::string, value_, cloned->value_ = value_; ))
                    )
            };



            //
            // Array Value
            //
            struct array_value RILL_CXX11_FINAL
                : public value_base
            {
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


                //////////////////////////////////////////////////
                RILL_MAKE_AST_DERIVED(
                    array_value, value_base,
                    (( expression_list, elements_list_,
                       for( auto const& e : elements_list_ )
                           cloned->elements_list_.push_back(
                               clone_ast<expression_ptr>( e )
                               );
                        ))
                    )
            };

        } // namespace intrinsic





        //
        //
        //
        struct nested_identifier_value RILL_CXX11_FINAL
            : public value
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                nested_identifier_value,
                (( std::vector<identifier_value_base_ptr>, ids_,
                   for( auto const& v : ids_ )
                       cloned->ids_.push_back(
                           clone_ast<identifier_value_base_ptr>( v )
                           );
                    ))
                (( bool, started_from_root_, cloned->started_from_root_ = started_from_root_; ))
                )
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
            : public value
        {
        public:
            identifier_value_base(
                intrinsic::symbol_value_ptr const& name,
                bool const started_from_root = false
                )
                : name_symbol_( name )
                , started_from_root_( started_from_root )
            {
#ifdef RILL_DEBUG
                std::cout << "IDENTIFIER INSTNCED: " << this << " / " << name->to_native_string() << std::endl;
#endif
            }

            virtual ~identifier_value_base() {}

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


            //////////////////////////////////////////////////
            RILL_MAKE_AST_INTERFACE(
                identifier_value_base,
                (( intrinsic::symbol_value_ptr, name_symbol_ ))
                (( bool, started_from_root_,
                   cloned->started_from_root_ = started_from_root_; ))
                )
        };



        //
        //
        //
        struct identifier_value RILL_CXX11_FINAL
            : public identifier_value_base
        {
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

            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                identifier_value, identifier_value_base
                )
        };



        inline auto make_identifier( native_string_t const& simple_typename )
            -> identifier_value_ptr
        {
            return std::make_shared<identifier_value>( simple_typename );
        }

        // deprcated
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
            intrinsic::symbol_value_ptr const& symbol_name
            )
            -> identifier_value_ptr
        {
            return make_binary_operator_identifier( symbol_name->to_native_string() );
        }
        // TODO: add overload function that implement template specified operator

        inline auto make_binary_operator_symbol(
            intrinsic::symbol_value_ptr const& symbol_name
            )
            -> intrinsic::symbol_value_ptr
        {
            return intrinsic::make_symbol(
                "%binary%operator_" + symbol_name->to_native_string()
                );
        }




        class template_instance_value RILL_CXX11_FINAL
            : public identifier_value_base
        {
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


            //////////////////////////////////////////////////
            RILL_MAKE_AST_DERIVED(
                template_instance_value, identifier_value_base,
                (( expression_list, template_args_,
                   for( auto const& e : template_args_ )
                       cloned->template_args_.push_back(
                           clone_ast<expression_ptr>( e )
                           );
                    ))
                )
        };

#if 0
        //
        //
        //
        struct literal_value RILL_CXX11_FINAL
            : public value
        {
        public:
            // specify value's type name
            literal_value( intrinsic::value_base_ptr const& bv )
                : holder_( bv )
                , literal_type_name_(
                    std::make_shared<identifier_value>( bv->get_native_typename_string() )
                    )
            {}

        public:
            virtual bool is_intrinsic() const RILL_CXX11_FINAL
            {
                return true;
            }


            //////////////////////////////////////////////////
            RILL_MAKE_AST(
                literal_value,
                (( intrinsic::value_base_ptr, holder_ ))
                (( const_identifier_value_ptr, literal_type_name_ ))
                )
        };
#endif

    } // namespace ast
} // namespace rill
