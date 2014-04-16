//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_COMPILE_TIME_LLVM_ENGINE_VALUE_STORAGE_HPP
#define RILL_COMPILE_TIME_LLVM_ENGINE_VALUE_STORAGE_HPP

#include <memory>

#include "../../config/macros.hpp"


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            auto make_dynamic_storage( std::size_t const& size, std::size_t const& align )
                -> std::shared_ptr<char>;

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill

#endif /*RILL_COMPILE_TIME_LLVM_ENGINE_VALUE_STORAGE_HPP*/
