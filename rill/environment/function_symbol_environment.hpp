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
#include "attributes_mixin.hpp"
#include "../behavior/intrinsic_action_holder_fwd.hpp"


namespace rill
{
    //
    // environments for normal function
    // per a function
    //
    class function_symbol_environment RILL_CXX11_FINAL
        : public environment_base, public attributes_mixin
    {
    public:
        static kind::type_value const KindValue;

    public:
        // pre construct
        function_symbol_environment(
            environment_parameter_t&& pp,
            weak_multiple_set_environment_ptr const& multiset_env_ptr,
            native_string_type const& base_name
            )
            : environment_base( std::move( pp ) )
            , multiset_env_ptr_( multiset_env_ptr )
            , base_name_( base_name )
            , return_type_id_( type_id_undefined )
            , is_initializer_function_( false )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
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

        auto get_return_type_id() const
            -> type_id_t
        {
            return return_type_id_;
        }

        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "function_symbol_environment[ "
                << base_name_ << " : " << mangled_name_ << " ]" << std::endl;
            return dump_include_env( os, indent );
        }

        auto append_parameter_variable(
            variable_symbol_environment_ptr const& v_env
            )
            -> void;

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

        void mark_as_initialize_function()
        {
            is_initializer_function_ = true;
        }

        bool is_initializer() const
        {
            return is_initializer_function_;
        }

        void mark_as_virtual( std::size_t const& index )
        {
            virtual_index_ = index;
        }

        auto is_virtual() const
            -> bool
        {
            return virtual_index_ != boost::none;
        }

        auto get_virtual_index() const
            -> std::size_t const&
        {
            assert( is_virtual() );
            return *virtual_index_;
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

        auto get_multiset_env()
        {
            return multiset_env_ptr_.lock();
        }

    private:
        weak_multiple_set_environment_ptr multiset_env_ptr_;

        native_string_type base_name_, mangled_name_;

        // parameter variable environments
        environment_id_list_t parameter_decl_ids_;

        // types
        type_id_list_t parameter_type_ids_;
        type_id_t return_type_id_;

        type_id_list_t return_type_candidates_;

        bool is_initializer_function_;
        boost::optional<std::size_t> virtual_index_;

        boost::optional<intrinsic_action_id_t> intrinsic_action_id_;
    };

} // namespace rill
