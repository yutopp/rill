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

#include "environment_base.hpp"
#include "global_environment.hpp"
#include "../behavior/intrinsic_action_holder_fwd.hpp"


namespace rill
{
    //
    // environments for normal function
    // per a function
    //
    class function_symbol_environment RILL_CXX11_FINAL
        : public environment_base
    {
    public:
        static kind::type_value const KindValue;

    public:
        // pre construct
        function_symbol_environment(
            environment_parameter_t&& pp,
            environment_id_t const& parameter_wrapper_env_id,
            native_string_type const& base_name
            )
            : environment_base( std::move( pp ) )
            , parameter_wrapper_env_id_( parameter_wrapper_env_id )
            , parent_class_env_id_( environment_id_undefined )
            , return_type_id_( type_id_undefined )
            , base_name_( base_name )
            , decl_attr_( attribute::decl::k_default )
            , is_initializer_function_( false )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
        }

        auto get_parameter_wrapper_env()
            -> multiple_set_environment_ptr
        {
            return cast_to<multiple_set_environment>(
                b_.lock()->get_env_at_as_strong_ref( parameter_wrapper_env_id_ )
                );
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

        auto decide_return_type( type_id_t const& return_type_id )
            -> void
        {
            return_type_id_ = return_type_id;
        }

        auto is_return_type_decided() const
            -> bool
        {
            return return_type_id_ != type_id_undefined;
        }

        auto complete(
            native_string_type const& mangled_name,
            attribute::decl::type const& decl_attr = attribute::decl::k_default
            )
            -> void
        {
            assert( is_return_type_decided() );
            mangled_name_ = mangled_name;
            set_attribute( decl_attr );

            change_progress_to_completed();
        }

        auto add_return_type_candidate( type_id_t const& type_id )
            -> void
        {
            rill_dout << "add overload candidate : " << type_id << " -> " << base_name_ << std::endl;
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
            os  << indent << "function_symbol_environment[ "
                << base_name_ << " : " << mangled_name_ << " ]" << std::endl;
            return dump_include_env( os, indent );
        }

        auto parameter_variable_construct(
            ast::identifier_value_base_ptr const& name,
            const_class_symbol_environment_ptr const& type_env,
            attribute::type_attributes const& type_attr = attribute::make_default_type_attributes()
            )
            -> variable_symbol_environment_ptr;

        auto parameter_variable_construct(
            ast::identifier_value_base_ptr const& name,
            environment_id_t const& type_env_id,
            attribute::type_attributes const& type_attr = attribute::make_default_type_attributes()
            )
            -> variable_symbol_environment_ptr;

        auto parameter_variable_construct(
            ast::identifier_value_base_ptr const& name,
            type_id_t const& type_id
            )
            -> variable_symbol_environment_ptr;

        auto get_base_name() const
            -> native_string_type const&
        {
            return base_name_;
        }

        auto get_mangled_name() const
            -> native_string_type const&
        {
            return mangled_name_;
        }

        auto unique_key_for_overload() const
            ->  native_string_type
        {
            return mangled_name_;
        }

        bool is_return_type_completed() const
        {
            return return_type_id_ != type_id_undefined;
        }

        bool has_attribute( attribute::decl::type const& attribute ) const
        {
            return ( decl_attr_ & attribute  ) != 0;
        }

        void set_attribute( attribute::decl::type const& attribute )
        {
            decl_attr_ |= attribute;
        }

        void unset_attribute( attribute::decl::type const& attribute )
        {
            decl_attr_ ^= attribute;
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
            return parent_class_env_id_ != environment_id_undefined;
        }

        void mark_as_initialize_function()
        {
            is_initializer_function_ = true;
        }

        bool is_initializer() const
        {
            return is_initializer_function_;
        }

        void mark_as_intrinsic_function( intrinsic_action_id_t const& id )
        {
            intrinsic_action_id_ = id;
        }

        auto get_action_id() const
            -> intrinsic_action_id_t const&
        {
            assert( intrinsic_action_id_ != boost::none );
            return *intrinsic_action_id_;
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

        native_string_type base_name_, mangled_name_;
        attribute::decl::type decl_attr_;

        bool is_initializer_function_;

        boost::optional<intrinsic_action_id_t> intrinsic_action_id_;
    };

} // namespace rill
