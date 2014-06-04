//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <algorithm>
#include <string>
#include <iostream>
#include <cassert>
#include <cstring>

#include <execinfo.h>
#include <cxxabi.h>


namespace rill
{
    namespace debug
    {
        auto dump_backtrace()
            -> void
        {
            void* array[50];
            auto const size = backtrace( array, 50 );

            std::cerr << __func__ << " backtrace returned " << size << " frames" << std::endl << std::endl;

            char** messages = backtrace_symbols( array, size );
            assert( messages!=nullptr );

            for( int i=0; i<size; ++i ) {
                auto const* const message = messages[i];
                assert( message != nullptr );

                auto const* const message_begin = message;
                auto const* const message_end = message + std::strlen( message );


                std::cout << "===" << std::endl;

                auto const open_paren = std::find( message_begin, message_end, '(' );
                if ( open_paren != message_end ) {
                    auto const close_paren = [&] {
                        {
                            auto it = std::find( open_paren, message_end, '+' );
                            if ( it != message_end ) return it;
                        }

                        {
                            auto it = std::find( open_paren, message_end, ')' );
                            if ( it != message_end ) return it;
                        }

                        return message_end;
                    }();

                    if ( close_paren != message_end && std::distance( open_paren + 1, close_paren ) > 0 ) {
                        std::string const mangled_name( open_paren + 1, close_paren );

                        int status;
                        char* const realname = abi::__cxa_demangle( mangled_name.c_str(), nullptr, nullptr, &status );
                        if ( status == 0 ) {
                            assert( realname != nullptr );
                            std::cout << realname << std::endl;

                        } else {
                            std::cout << "demangle failed... : " << mangled_name << std::endl;
                        }
                        if ( realname != nullptr ) std::free( realname );

                        continue;
                    }
                }

                // if reached there, show the raw message
                std::cerr << "[bt]: (" << i << ") " << message << std::endl;
            }
            std::cerr << std::endl;

            std::free( messages );
        }

    } // namespace debug
} // namespace rill
