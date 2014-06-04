//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP
#define RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP

#include <cassert>
#include <memory>

#include "environment_base.hpp"


namespace rill
{
    //
    // template environment
    // this environment holds template parameters
    // and bacomes the POINT linked to TEMPLATE symbol's AST.
    // So, to get inner AST(AST of class, function, etc...), do like below.
    //   1. get the template AST(e.g. named 's'), that is linked from this env
    //   2. call s.get_inner_statement() member function
    //
    class template_environment RILL_CXX11_FINAL
        : public environment_base
    {
    public:
        static kind::type_value const KindValue = kind::type_value::e_template;

        typedef std::size_t     template_argument_length_type;
        static template_argument_length_type const variadic_length = -1;

    public:
        template_environment( environment_parameter_t&& pp, environment_id_t const& template_set_env_id )
            : environment_base( std::move( pp ) )
            , parameter_num_( 0 )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_template;
        }


        auto set_parameter_num( std::size_t const& n )
            -> void
        {
            parameter_num_ = n;
        }

        auto get_parameter_num() const
            -> std::size_t const&
        {
            return parameter_num_;
        }


        auto get_parameter_decl_ids() const
            -> environment_id_list_t const&
        {
            return parameter_decl_ids_;
        }

        auto get_parameter_type_ids() const
            -> type_id_list_t const&
        {
            return parameter_type_ids_;
        }

        auto parameter_variable_construct(
            ast::identifier_value_base_ptr const& name,
            const_class_symbol_environment_ptr const& type_env,
            attribute::type_attributes const& type_attr = attribute::make_default_type_attributes()
            )
            -> variable_symbol_environment_ptr;

    private:
        std::size_t parameter_num_;


        // parameter variable environments
        environment_id_list_t parameter_decl_ids_;

        // types
        type_id_list_t parameter_type_ids_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_TEMPLATE_ENVIRONMENT_HPP*/
