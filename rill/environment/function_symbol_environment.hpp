//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once


#include <cassert>
#include <memory>
#include <unordered_map>
#include <bitset>
#include <vector>
#include <utility>
#include <boost/range/adaptor/transformed.hpp>

#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "single_identifier_environment_base.hpp"
#include "has_parameter_environment.hpp"


namespace rill
{
    //
    // function
    //
    class function_symbol_environment RILL_CXX11_FINAL
        : public single_identifier_environment_base
    {
    public:
        static kind::type_value const KindValue = kind::type_value::e_function;

        enum attr : int {
            e_normal = 0,
            e_extern = 1 << 0
        };
        typedef int attributes_t;

    public:
        // pre construct
        function_symbol_environment( environment_parameter_t&& pp, environment_id_t const& parameter_wrapper_env_id )
            : single_identifier_environment_base( std::move( pp ) )
            , parameter_wrapper_env_id_( parameter_wrapper_env_id )
            , parent_class_env_id_( environment_id_undefined )
            , return_type_id_( type_id_undefined )
            , attributes_( e_normal )
        {}

    public:
        virtual auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
        }



        auto get_parameter_wrapper_env()
            -> std::shared_ptr<has_parameter_environment<function_symbol_environment>>
        {
            return std::static_pointer_cast<has_parameter_environment<function_symbol_environment>>( get_env_at( parameter_wrapper_env_id_ ).lock() );
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



        auto complete(
            type_id_t const& return_type_id,
            native_string_type const& name,
            attributes_t const& attrbute = attr::e_normal
            )
            -> void
        {
            return_type_id_ = return_type_id;
            name_ = name;
            attributes_ = attrbute;

            change_progress_to_completed();
        }

        auto add_return_type_candidate( type_id_t const& type_id )
            -> void
        {
            return_type_candidates_.push_back( type_id );
        }

        auto get_return_type_id() const
            -> type_id_t
        {
            return return_type_id_;
        }

        auto get_return_type_candidates() const
            -> type_id_list_t const&
        {
            return return_type_candidates_;
        }

        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "function_symbol_environment" << std::endl;
            return dump_include_env( os, indent );
        }

        auto parameter_variable_construct(
            ast::identifier_value_base_ptr const& name,
            const_class_symbol_environment_ptr const& type_env,
            attribute::type_attributes const& type_attr = attribute::make_default_type_attributes()
            )
            -> variable_symbol_environment_ptr;

        auto mangled_name() const
            -> native_string_type;

        bool is_return_type_completed() const
        {
            return return_type_id_ != type_id_undefined;
        }

        bool has_attribute( attr const& attribute ) const
        {
            return ( attributes_ & attribute  ) != 0;
        }

        void set_parent_class_env_id( environment_id_t const& parent_class_env_id )
        {
            parent_class_env_id_ = parent_class_env_id;
        }

        auto get_parent_class_env_id() const
            -> environment_id_t
        {
            return parent_class_env_id_;
        }

        bool is_in_class() const
        {
            return parent_class_env_id_ == environment_id_undefined;
        }

    private:
        environment_id_t parameter_wrapper_env_id_;
        environment_id_t parent_class_env_id_;

        // parameter variable environments
        environment_id_list_t parameter_decl_ids_;

        // types
        type_id_list_t parameter_type_ids_;
        type_id_t return_type_id_;

        type_id_list_t return_type_candidates_;

        native_string_type name_;
        attributes_t attributes_;
    };

} // namespace rill
