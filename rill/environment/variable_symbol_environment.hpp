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

#include "environment_fwd.hpp"


namespace rill
{
    //
    // variable
    //
    class variable_symbol_environment RILL_CXX11_FINAL
        : public single_identifier_environment_base
    {
    public:
        static kind::type_value const KindValue = kind::type_value::variable_e;

    public:
        variable_symbol_environment( environment_id_t const& id, weak_env_pointer const& parent )
            : single_identifier_environment_base( id, parent )
    //        , value_type_env_id_( envitonment_id_undefined )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
        }

        auto is_incomplete() const
            -> bool RILL_CXX11_OVERRIDE
        {
            return true;//value_type_env_id_ == envitonment_id_undefined;
        }

    /*    auto get_weak_type_env()
            -> weak_environment_ptr
        {
            return get_env_at( value_type_env_id_ );
        }
        auto get_weak_type_env() const
            -> const_weak_environment_ptr
        {
            return get_env_at( value_type_env_id_ );
        }*/

        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os << indent << "varialbe_environment" << std::endl;
    //        os << indent << "  = return type  " << value_type_env_id_ << std::endl;
            return dump_include_env( os, indent );
        }

    private:
    //    environment_id_t value_type_env_id_;
    };

} // namespace rill
