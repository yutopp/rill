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

#include <boost/strong_typedef.hpp>


namespace rill
{
    BOOST_STRONG_TYPEDEF( std::size_t, type_id_t );
    type_id_t const type_id_limit = type_id_t( std::numeric_limits<std::size_t>::max() - 2 );
    type_id_t const type_id_special = type_id_t( std::numeric_limits<std::size_t>::max() - 1 );
    type_id_t const type_id_undefined = type_id_t( std::numeric_limits<std::size_t>::max() );

    namespace detail
    {
        struct type_id_t_hasher
        {
            inline auto operator()( const type_id_t& t ) const
                -> std::size_t
            {
                return std::hash<std::size_t>()( t.t );
            }
        };
    } // namespace detail


    typedef std::vector<type_id_t> type_id_list_t;
    

    // type is composed to Class and Attributs
    struct type;

    //
    class type_registry;

} // namespace rill

#endif /*RILL_ENVIRONMENT_TYPE_REGISTRY_FWD_HPP*/
