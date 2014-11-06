//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <string>
#include <boost/filesystem/path.hpp>

#include "../ast.hpp"


namespace rill
{
    auto make_module_name(
        boost::filesystem::path const& base_path,
        ast::const_module_ptr const& module
        )
        -> std::string;

} // namespace rill
