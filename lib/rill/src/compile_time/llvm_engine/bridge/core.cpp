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


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            std::unordered_map<std::string, void*> const ctfe_intrinsic_function_table = {
                { "rill_abababa", reinterpret_cast<void*>( &rill_abababa ) },
                { "rill_core_typesystem_is_mutable", reinterpret_cast<void*>( rill_core_typesystem_is_mutable) }
            };

            static jit_execution_environmant gje;

            auto set_global_jit_execution_environment(
                jit_execution_environmant const& je
                )
                -> void
            {
                gje = je;
            }

            auto clear_global_jit_execution_environment()
                -> void
            {
                gje = jit_execution_environmant{
                    nullptr
                };
            }

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill


extern "C" {
    namespace le = rill::compile_time::llvm_engine;

    auto rill_abababa( rill::semantic_analysis::type_detail_ptr ty_detail )
        -> rill::semantic_analysis::type_detail_ptr
    {
        std::cout << "oioio ~~~ i" << std::endl;
        std::cout << "jit function call!" << std::endl;

        //
        rill::type t = le::gje.semantic_analyzer->ref_type( ty_detail );
        t.attributes <<= rill::attribute::modifiability_kind::k_mutable;

        return le::gje.semantic_analyzer->qualify_type(
            ty_detail,
            t.attributes
            );
    }

    auto rill_core_typesystem_is_mutable( rill::semantic_analysis::type_detail_ptr ty_detail )
        -> bool
    {
        std::cout << "POINTER:" << ty_detail << std::endl;

        rill::type const& t = le::gje.semantic_analyzer->ref_type( ty_detail );
        bool const is_mutable = t.attributes.modifiability == rill::attribute::modifiability_kind::k_mutable;

        return is_mutable;
    }
}
