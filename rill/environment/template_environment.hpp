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
#if 0
    class template_environment RILL_CXX11_FINAL
        : public environment
    {
        template<typename T>
        friend class environment_allocator;

    public:
        template_environment( environment_id_t const& id, weak_env_pointer const& parent, void* )
            : environment( id, parent )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::none_e; // TODO: change to template_e
        }

    private:
        std::unordered_map<native_string_type, environment_ptr> simple_env_;
    };
#endif

} // namespace rill
