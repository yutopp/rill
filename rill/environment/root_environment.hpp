//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "environment_base.hpp"


namespace rill
{
    //
    // for root
    //
    class root_environment RILL_CXX11_FINAL
        : public environment_base
    {
    public:
        root_environment()
            : environment_base( root_initialize_tag() )
        {}

    private:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_none;
        }

        bool is_root() const RILL_CXX11_OVERRIDE
        {
            return true;
        }

    private:

    };

} // namespace rill
