//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine/value_storage.hpp>

#include <memory>
#include <utility>
#include <cstdint>


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            auto make_dynamic_storage( std::size_t const& size, std::size_t const& align )
                -> std::shared_ptr<char>
            {
                // calculate enough memory size
                auto const raw_buffer_size = ( size + ( RILL_MAX_ALIGN - 1 ) ) / RILL_MAX_ALIGN;
                //
                std::shared_ptr<char> raw_buffer( new char[raw_buffer_size], std::default_delete<char[]>() );
                std::uintptr_t const raw_buffer_address = reinterpret_cast<std::uintptr_t>( raw_buffer.get() );

                //
                auto const offset = ( ( raw_buffer_address % align ) == 0 ) ? 0 : ( align - ( raw_buffer_address % align ) );
                // auto const buffer_size = raw_buffer_size - offset;
                auto const buffer_address = raw_buffer_address + offset;

                // buffer that pointing to aligned address
                std::shared_ptr<char> buffer( raw_buffer, reinterpret_cast<char*>( buffer_address ) );

                return buffer;
            }

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill
