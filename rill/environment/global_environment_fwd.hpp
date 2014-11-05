//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <memory>


namespace rill
{
    class global_environment;
    using global_environment_ptr = std::shared_ptr<global_environment>;
    using const_global_environment_ptr = std::shared_ptr<global_environment const>;

    using weak_global_environment_ptr = std::weak_ptr<global_environment>;

} // namespace rill
