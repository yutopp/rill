//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_MESSAGE_DUMMY_MESSAGE_CONTAINER_HPP
#define RILL_MESSAGE_DUMMY_MESSAGE_CONTAINER_HPP

namespace rill
{
    namespace message
    {
        class dummy_message_container
        {
        public:
            using message_type = void*;

        public:
            inline auto is_error_state() const
                -> bool
            {
                return false;
            }

            inline auto is_error_state( bool const ) const
                -> void
            {}

            template<typename T>
            inline auto save_message( T&& ) const
                -> void
            {}

        public:
            inline auto get_report() const
                -> void*
            {
                return nullptr;
            }
        };

    } // namespace message
} // namespace rill

#endif /*RILL_MESSAGE_DUMMY_MESSAGE_CONTAINER_HPP*/
