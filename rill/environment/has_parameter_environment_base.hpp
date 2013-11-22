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

#include "environment.hpp"


namespace rill
{
    //
    // 
    //
    class has_parameter_environment_base
        : public environment
    {
    public:
        has_parameter_environment_base( environment_id_t const id, weak_env_pointer const& parent )
            : environment( id, parent )
        {}

        virtual ~has_parameter_environment_base() {}

    public:
        virtual auto get_inner_symbol_kind() const
            -> kind::type_value =0;
     /*
        virtual auto add_overload( parameter_list const& parameter, function_definition_statement_base_ptr const& sp* )
            -> env_pointer =0;
   
        virtual auto lookup( environment_ptr const& parent, parameter_list const& parameter ) const
            -> const_environment_ptr =0;*/
    };

} // namespace rill
