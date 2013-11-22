//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <string>
#include <memory>

#include "../ast/root_fwd.hpp"


namespace rill
{
    namespace syntax_analysis
    {
        typedef std::string                     input_type;
        typedef input_type::const_iterator      input_iterator;


        auto make_syntax_tree( input_type const& source ) -> ast::root_ptr;

    } // namespace syntax_analysis
} // namespace rill