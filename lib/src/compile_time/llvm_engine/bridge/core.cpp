//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine/bridge.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

// symbols will be solved by LLVM
#include <rill-rt/lib/runtime.hpp>

#include <iostream>


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            std::unordered_map<std::string, void*> const ctfe_intrinsic_function_table = {
                {
                    "rill_core_typesystem_mutable",
                    reinterpret_cast<void*>( &rill_core_typesystem_mutable )
                },
                {
                    "rill_core_typesystem_const",
                    reinterpret_cast<void*>( &rill_core_typesystem_const )
                },
                {
                    "rill_core_typesystem_immutable",
                    reinterpret_cast<void*>( &rill_core_typesystem_immutable )
                },
                {
                    "rill_core_typesystem_ref",
                    reinterpret_cast<void*>( &rill_core_typesystem_ref )
                },
                {
                    "rill_core_typesystem_is_mutable",
                    reinterpret_cast<void*>( rill_core_typesystem_is_mutable )
                },

                // TODO: find automatically
                {
                    "print_bytes",
                    reinterpret_cast<void*>( ::print_bytes )
                },
                {
                    "print_int32",
                    reinterpret_cast<void*>( ::print_int32 )
                }

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

    auto rill_core_typesystem_mutable( rill::type_detail_ptr ty_detail )
        -> rill::type_detail_ptr
    {
        rill::type t = le::gje.semantic_analyzer->ref_type( ty_detail );
        t.attributes <<= rill::attribute::modifiability_kind::k_mutable;

        return le::gje.semantic_analyzer->qualify_type(
            ty_detail,
            t.attributes
            );
    }

    auto rill_core_typesystem_const( rill::type_detail_ptr ty_detail )
        -> rill::type_detail_ptr
    {
        rill::type t = le::gje.semantic_analyzer->ref_type( ty_detail );
        t.attributes <<= rill::attribute::modifiability_kind::k_const;

        return le::gje.semantic_analyzer->qualify_type(
            ty_detail,
            t.attributes
            );
    }

    auto rill_core_typesystem_immutable( rill::type_detail_ptr ty_detail )
        -> rill::type_detail_ptr
    {
        rill::type t = le::gje.semantic_analyzer->ref_type( ty_detail );
        t.attributes <<= rill::attribute::modifiability_kind::k_immutable;

        return le::gje.semantic_analyzer->qualify_type(
            ty_detail,
            t.attributes
            );
    }

    auto rill_core_typesystem_ref( rill::type_detail_ptr ty_detail )
        -> rill::type_detail_ptr
    {
        rill::type t = le::gje.semantic_analyzer->ref_type( ty_detail );
        t.attributes <<= rill::attribute::holder_kind::k_ref;

        return le::gje.semantic_analyzer->qualify_type(
            ty_detail,
            t.attributes
            );
    }

    auto rill_core_typesystem_is_mutable( rill::type_detail_ptr ty_detail )
        -> bool
    {
        rill_dout << "POINTER:" << ty_detail << std::endl;

        rill::type const& t = le::gje.semantic_analyzer->ref_type( ty_detail );
        bool const is_mutable = t.attributes.modifiability == rill::attribute::modifiability_kind::k_mutable;

        return is_mutable;
    }
}
