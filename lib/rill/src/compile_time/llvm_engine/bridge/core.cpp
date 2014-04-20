//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine/bridge.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

#include <iostream>


extern "C" {
    auto rill_abababa( rill::semantic_analysis::type_detail_ptr ty_detail )
        -> rill::semantic_analysis::type_detail_ptr
    {
        std::cout << "feife~~i" << std::endl;
        std::cout << "jit function call!" << std::endl;

        return ty_detail;
    }
}
