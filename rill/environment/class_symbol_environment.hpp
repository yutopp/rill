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

#include "../config/macros.hpp"

#include "environment_base.hpp"
#include "class_symbol_builtin_kind.hpp"


namespace rill
{
    enum class class_traits_kind : std::size_t
    {
        k_has_non_trivial_copy_ctor,
        k_has_non_trivial_move_ctor,
        k_has_non_trivial_copy_assign,
        k_has_non_trivial_move_assign,
        k_has_non_trivial_dtor,
        k_has_non_default_copyable_member,

        k_has_non_immutable_alias,

        last
    };

    //
    // class
    //
    class class_symbol_environment final
        : public environment_base
    {
    public:
        static kind::type_value const KindValue;

    public:
        class_symbol_environment(
            environment_parameter_t&& pp,
            environment_id_t const& wrapper_set_env_id,
            native_string_type const& base_name
            )
            : environment_base( std::move( pp ) )
            , base_name_( base_name )
            , decl_attr_( attribute::decl::k_default )
            , builtin_kind_( class_builtin_kind::k_none )
            , host_align_( std::numeric_limits<std::size_t>::max() )
            , host_size_( std::numeric_limits<std::size_t>::max() )
            , target_align_( std::numeric_limits<std::size_t>::max() )
            , target_size_( std::numeric_limits<std::size_t>::max() )
            , traits_{}
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value override
        {
            return KindValue;
        }

        auto complete(
            native_string_type const& mangled_name,
            attribute::decl::type const& decl_attr = attribute::decl::k_default
            )
            -> void
        {
            mangled_name_ = mangled_name;
            set_attribute( decl_attr );

            change_progress_to_completed();
        }

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

        auto set_builtin_kind( class_builtin_kind const& kind )
        {
            builtin_kind_ = kind;
        }

        auto get_builtin_kind() const
            -> class_builtin_kind const&
        {
            return builtin_kind_;
        }

        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "class_symbol_environment" << std::endl;
            return dump_include_env( os, indent );
        }

    public:
        auto make_as_array(
            type_id_t const& inner_type_id,
            std::size_t const& elements_num
            )
            -> void
        {
            assert( pointer_detail_ == nullptr );
            array_detail_ = std::make_shared<array_detail>( inner_type_id, elements_num );
        }

        auto is_array() const
            -> bool
        {
            return array_detail_ != nullptr;
        }

        auto make_as_pointer(
            type_id_t const& inner_type_id
            )
            -> void
        {
            assert( array_detail_ == nullptr );
            pointer_detail_ = std::make_shared<pointer_detail>( inner_type_id );
        }

        auto is_pointer() const
            -> bool
        {
            return pointer_detail_ != nullptr;
        }

    public:
        struct array_detail
        {
            array_detail(
                type_id_t const& i,
                std::size_t const& e
                )
                : inner_type_id( i )
                , elements_num( e )
            {}

            type_id_t inner_type_id;
            std::size_t elements_num;
        };

        auto get_array_detail() const
            -> std::shared_ptr<array_detail const>
        {
            return array_detail_;
        }

    public:
        struct pointer_detail
        {
            pointer_detail(
                type_id_t const& i
                )
                : inner_type_id( i )
            {}

            type_id_t inner_type_id;
        };

        auto get_pointer_detail() const
            -> std::shared_ptr<pointer_detail const>
        {
            return pointer_detail_;
        }

    public:
        auto set_host_align( std::size_t const& s )
            -> void
        {
            host_align_ = s;
        }

        auto set_host_size( std::size_t const& s )
            -> void
        {
            host_size_ = s;
        }

        auto set_target_align( std::size_t const& s )
            -> void
        {
            target_align_ = s;
        }

        auto set_target_size( std::size_t const& s )
            -> void
        {
            target_size_ = s;
        }


        auto get_host_align() const
        {
            assert( host_align_ != std::numeric_limits<std::size_t>::max() );
            return host_align_;
        }

        auto get_host_size() const
        {
            assert( host_size_ != std::numeric_limits<std::size_t>::max() );
            return host_size_;
        }

        auto get_target_align() const
        {
            assert( target_align_ != std::numeric_limits<std::size_t>::max() );
            return target_align_;
        }

        auto get_target_size() const
        {
            assert( target_size_ != std::numeric_limits<std::size_t>::max() );
            return target_size_;
        }

    private:
        native_string_type base_name_, mangled_name_;
        attribute::decl::type decl_attr_;
        class_builtin_kind builtin_kind_;

        std::shared_ptr<array_detail> array_detail_;
        std::shared_ptr<pointer_detail> pointer_detail_;

        std::size_t host_align_, host_size_;
        std::size_t target_align_, target_size_;

    public:
        inline auto set_traits_flag( class_traits_kind const& f, bool const b )
            -> void
        {
            traits_[static_cast<std::size_t>( f )] = b;
        }

        inline auto has_traits_flag( class_traits_kind const& f ) const
            -> bool
        {
            return traits_[static_cast<std::size_t>( f )];
        }

        auto is_default_copyable() const
            -> bool;

        auto is_immutable() const
            -> bool;

    private:
        bool traits_[static_cast<std::size_t>( class_traits_kind::last )];
    };

} // namespace rill
