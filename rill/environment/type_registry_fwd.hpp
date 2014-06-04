//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_TYPE_REGISTRY_FWD_HPP
#define RILL_ENVIRONMENT_TYPE_REGISTRY_FWD_HPP

#include <vector>
#include <limits>

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

    // TODO: change to strong typedef
    typedef std::size_t type_id_t;

    //
    auto const type_id_limit = type_id_t( std::numeric_limits<std::size_t>::max() - 20 );
    auto const type_id_special = type_id_t( std::numeric_limits<std::size_t>::max() - 19 );
    auto const type_id_special_limit = type_id_t( std::numeric_limits<std::size_t>::max() - 1 );
    auto const type_id_undefined = type_id_t( std::numeric_limits<std::size_t>::max() );

    // -18 ~ -2
    enum struct type_id_nontype : type_id_t
    {
        e_function = type_id_t( std::numeric_limits<std::size_t>::max() - 18 ),
        e_template_class = type_id_t( std::numeric_limits<std::size_t>::max() - 17 ),
        e_namespace = type_id_t( std::numeric_limits<std::size_t>::max() - 16 )
    };

    inline auto is_nontype_id( type_id_t const& tid )
        -> bool
    {
        return tid >= type_id_special && tid < type_id_special_limit;
    }

    inline auto is_type_id( type_id_t const& tid )
        -> bool
    {
        return !is_nontype_id( tid );
    }


    namespace detail
    {
        struct type_id_t_hasher
        {
            inline auto operator()( const type_id_t& t ) const
                -> std::size_t
            {
                return std::hash<std::size_t>()( t /*t.t*/ );
            }
        };
    } // namespace detail


    typedef std::vector<type_id_t> type_id_list_t;


    // type is composed to Class and Attributs

    //
    class type_registry;

} // namespace rill

#endif /*RILL_ENVIRONMENT_TYPE_REGISTRY_FWD_HPP*/
