//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_TYPE_TYPE_REGISTRY_HPP
#define RILL_TYPE_TYPE_REGISTRY_HPP

#include <unordered_map>

#include <boost/optional.hpp>

#include "type_registry_fwd.hpp"
#include "type.hpp"
#include "../environment/environment_fwd.hpp"

#include "../debug/debug.hpp"


namespace rill
{
    //
    class type_registry
    {
    public:
        typedef type_id_t                   type_id_type;
        typedef type                        type_type;

        typedef std::allocator<type_type>   dereference_allocator_type;

    public:
        template<typename ClassEnvPtr>
        auto add(
            ClassEnvPtr const& class_env,
            attribute::type_attributes const& type_attr
            )
            -> type_id_t
        {
            auto const& class_env_id
                = ( class_env != nullptr )
                ? class_env->get_id()
                : environment_id_undefined;

            return add( class_env_id, type_attr );
        }

        auto add(
            environment_id_t const& class_env_id,
            attribute::type_attributes const& type_attr
            )
            -> type_id_t
        try {
            if ( auto const& o = is_exist( class_env_id, type_attr ) ) {
                return *o;
            }

            // make a type
            type t = { class_env_id, type_attr };
            data_holder_.emplace( current_index_, std::move( t ) );

            rill_dout << ">>> type registered." << std::endl
                      << ">>>  id  : " << class_env_id << std::endl;

            dereference_data_holder_[class_env_id][attribute::detail::make_type_attributes_bit( type_attr )] = current_index_;

            return type_id_t( current_index_++ );

        } catch( std::exception const& e ) {
            std::cerr << e.what() << std::endl;
            assert( false && "[ice]" );
        }


        auto at( type_id_t const& type_id )
            -> type_type&
        {
            if ( type_id >= type_id_limit ) {
                rill::debug::dump_backtrace();
                assert( false && "[[ICE]]" );
            }

            return data_holder_.at( type_id );
        }

        auto at( type_id_t const& type_id ) const
            -> type_type const&
        {
            if ( type_id >= type_id_limit ) {
                rill::debug::dump_backtrace();
                assert( false && "[[ICE]]" );
            }

            return data_holder_.at( type_id );
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

#endif /*RILL_TYPE_TYPE_REGISTRY_HPP*/
