//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_LLVM_ENGINE_VALUE_CONVERTER_HPP
#define RILL_COMPILE_TIME_LLVM_ENGINE_VALUE_CONVERTER_HPP

#include <llvm/IR/DerivedTypes.h>
#include <llvm/ExecutionEngine/GenericValue.h>


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            auto convert_storage_to_generic_value(
                void* const storage,
                llvm::Type const* const
                ) -> llvm::GenericValue;

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_VALUE_CONVERTER_HPP*/
