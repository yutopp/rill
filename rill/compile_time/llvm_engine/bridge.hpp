//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_LLVM_ENGINE_BRIDGE_HPP
#define RILL_COMPILE_TIME_LLVM_ENGINE_BRIDGE_HPP

#include <unordered_map>
#include <string>

#include "../../semantic_analysis/type_detail.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            extern std::unordered_map<std::string, void*> const ctfe_intrinsic_function_table;

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill


extern "C" {
    auto rill_abababa( rill::semantic_analysis::type_detail_ptr ty_detail )
        -> rill::semantic_analysis::type_detail_ptr;
}

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_BRIDGE_HPP*/
