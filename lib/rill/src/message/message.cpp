//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>

#include <rill/message/message.hpp>


namespace rill
{
    namespace message
    {
        auto print( message_object const& msg )
            -> void
        {
            std::cout << "ABYAAA" << std::endl;
        }

    } // namespace message
} // namespace rill
