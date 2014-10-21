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

#include <boost/core/demangle.hpp>

#include <rill/utility/colorize.hpp>


namespace rill
{
    namespace debug
    {
        auto dump_backtrace()
            -> void
        {
            void* array[50];
            auto const size = ::backtrace( array, 50 );

            std::cerr << colorize::standard::fg::red
                      << "========================================" << std::endl
                      << " Backtrace " << size << " frames" << std::endl
                      << "========================================" << std::endl
                      << colorize::standard::reset;

            char** messages = backtrace_symbols( array, size );
            assert( messages!=nullptr );

            for( int i=0; i<size; ++i ) {
                char const* const raw_message = messages[i];
                assert( raw_message != nullptr );

                std::string const message( raw_message );

                std::cerr << "[depth]: (" << i << ")" << std::endl;

                auto const func_first = message.find_first_of('(');
                auto const func_last = message.find_last_of('+');
                if ( func_first != std::string::npos && func_last != std::string::npos ) {
                    auto const function_name = std::string( message.cbegin() + func_first + 1, message.cbegin() + func_last );
                    if ( function_name.size() != 0 ) {
                        std::cerr << " function: "
                                  << colorize::standard::fg::green
                                  << boost::core::demangle( function_name.c_str() ) << std::endl
                                  << colorize::standard::reset;
                    }
                }
                std::cerr << " detail  : " << message << std::endl;
                std::cerr << "----------" << std::endl;
            }
            std::cerr << std::endl;

            std::free( messages );
        }

    } // namespace debug
} // namespace rill
