//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_DEBUG_HPP
#define RILL_DEBUG_HPP

#include <iostream>


namespace rill
{
    namespace debug
    {
        //
        template<typename T>
        void print_ast( T const& node )
        {
            node->dump( std::cout );
        }

        void dump_backtrace();

    } // namespace debug
} // namespace rill

#endif /*RILL_DEBUG_HPP*/
