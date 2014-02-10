//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_TYPE_REGISTRY_HPP
#define RILL_ENVIRONMENT_TYPE_REGISTRY_HPP

#include <unordered_map>

#include <boost/optional.hpp>

#include "type_registry_fwd.hpp"
#include "environment_fwd.hpp"
#include "../attribute/type.hpp"


namespace rill
{
    // type is composed to Class and Attributes
    struct type
    {
        environment_id_t class_env_id;
        attribute::type_attributes attributes;
    };

/*
    template<typename ClassEnvPtr>
    auto make_mangled_name(
        ClassEnvPtr const& class_env,
        attribute::type_attributes const& type_attr
        )
*/

    // type_id is reference id to type that attributed!

    //
    class type_registry
    {
    public:
        typedef type_id_t       type_id_type;
        typedef type            type_type;

        typedef std::allocator<type_type> dereference_allocator_type;

    public:
        auto add(
            environment_id_t const& class_env_id,
            attribute::type_attributes const& type_attr
            )
            -> type_id_t        
        try {
            // FIXME

            if ( auto const& o = is_exist( class_env_id, type_attr ) ) {
                return *o;
            }

            type t = { class_env_id, type_attr };
            data_holder_.emplace( current_index_, std::move( t ) );

            std::cout << ">>> type registered." << std::endl
                /*          << (const_environment_base_ptr)class_env << std::endl*/;

            dereference_data_holder_[class_env_id][attribute::detail::make_type_attributes_bit( type_attr )] = current_index_;

            return type_id_t( current_index_++ );

        } catch( std::exception const& e ) {
            std::cerr << e.what() << std::endl;
            assert( false && "[ice]" );
        }

        // (TODO: templated class OR) class
        template<typename ClassEnvPtr>
        auto add(
            ClassEnvPtr const& class_env,
            attribute::type_attributes const& type_attr
            )
            -> type_id_t        
        {
             // FIXME
            assert( class_env != nullptr );

            return add( class_env->get_id(), type_attr );
        }


        auto at( type_id_t const& type_id ) const
            -> type_type const&
        {
            return data_holder_.at( type_id );
        }


        auto get_has_same_class_types( environment_id_t const& class_env_id ) const
            -> std::unordered_map<attribute::attributes_bit_t, type_id_t> const&
        {
            return dereference_data_holder_.at( class_env_id );
        }


        auto is_class_exist( environment_id_t const& class_env_id ) const
            -> bool
        {
            return dereference_data_holder_.find( class_env_id ) != dereference_data_holder_.cend();
        }

        auto is_exist( environment_id_t const& class_env_id, attribute::type_attributes const& type_attr ) const
            -> boost::optional<type_id_t>
        {
            auto const it = dereference_data_holder_.find( class_env_id );
            if ( it == dereference_data_holder_.cend() )
                return boost::none;

            auto const& reit = it->second.find( attribute::detail::make_type_attributes_bit( type_attr ) );
            return ( reit != it->second.cend() ) ? boost::make_optional( reit->second ) : boost::none;
        }
/*
        // it makes be able to find id from env and attributes
        template<typename ClassEnvPtr>
        auto link_name_and_id(
            ClassEnvPtr const& class_env,
            attribute::type_attributes const& type_attr
            )
            -> type_id_t
        {
            type t = { class_env_id->get_id(), type_attr };
            data_holder_.emplace( current_index_, std::move( t ) );

            return type_id_t( current_index_++ );
        }
*/

        //inline auto is_exist()

    private:
        type_id_t current_index_ = type_id_t( 0 );
        std::unordered_map<type_id_t, type, detail::type_id_t_hasher> data_holder_;

        // TODO: use Scoped Allocator
        std::unordered_map<
            environment_id_t,
            std::unordered_map<attribute::attributes_bit_t, type_id_t>
            > dereference_data_holder_;

        //std::allocator<attributes_bit_t> dereference_data_allocator_;
    };

} // namespace rill

#endif /*RILL_ENVIRONMENT_TYPE_REGISTRY_HPP*/
