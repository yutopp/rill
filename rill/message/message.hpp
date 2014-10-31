//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_MESSAGE_MESSAGE_HPP
#define RILL_MESSAGE_MESSAGE_HPP

#include <exception>
#include <string>


namespace rill
{
    namespace message
    {
        enum class message_level
        {
            e_normal,
            e_warning,
            e_error
        };

        //
        struct message_object
        {
            message_level level;
            std::string message;
        };

        auto print( message_object const& )
            -> void;

    } // namespace message
} // namespace rill

#endif /*RILL_MESSAGE_MESSAGE_HPP*/
