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

#include "../../semantic_analysis/analyzer_fwd.hpp"
#include "../../semantic_analysis/type_detail.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            extern std::unordered_map<std::string, void*> const ctfe_intrinsic_function_table;

            //
            struct jit_execution_environmant
            {
                semantic_analysis::analyzer* semantic_analyzer;
            };

            //
            auto set_global_jit_execution_environment(
                jit_execution_environmant const&
                )
                -> void;

            auto clear_global_jit_execution_environment()
                -> void;

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill


extern "C" {
    auto rill_abababa( rill::semantic_analysis::type_detail_ptr ty_detail )
        -> rill::semantic_analysis::type_detail_ptr;

    auto rill_core_typesystem_is_mutable( rill::semantic_analysis::type_detail_ptr ty_detail )
        -> bool;
}

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_BRIDGE_HPP*/
