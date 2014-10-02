//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <memory>
#include <vector>
// for helper
#include <iostream>

#include "detail/base_type.hpp"
#include "value_def.ipp"


namespace rill
{
    namespace ast
    {
        //
        std::ostream& operator<<( std::ostream& os, value const& vp );

    } // namespace ast
} // namespace rill
