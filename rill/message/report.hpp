//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_MESSAGE_REPORT_HPP
#define RILL_MESSAGE_REPORT_HPP

#include <vector>
#include <stack>
#include <algorithm>

#include "message.hpp"


namespace rill
{
    namespace message
    {
        template<typename M>
        class report
        {
            using self_type = report<M>;
            using message_type = M;

        public:
            inline auto is_errored() const
                -> bool
            {
                return std::count_if( messages_.cbegin(), messages_.cend(), []( auto&& v ) {
                        return v.level == message::message_level::e_error;
                    }) > 0;
            }

            inline auto append_message( message_type&& msg )
                -> void
            {
                messages_.emplace_back( std::move( msg ) );
            }

            inline auto get_messages() const
                -> std::vector<message_type> const&
            {
                return messages_;
            }

        public:
            inline auto append_stocked_message( message_type&& msg )
                -> void
            {
                stocked_.emplace( std::move( msg ) );
            }

            inline auto get_stocked() const
                -> std::stack<message_type> const&
            {
                return stocked_;
            }

            inline auto get_stocked()
                -> std::stack<message_type>&
            {
                return stocked_;
            }

        public:
            auto import_from( self_type const& rep )
                -> void
            {
                std::copy(
                    rep.messages_.cbegin(),
                    rep.messages_.cend(),
                    std::back_inserter( messages_ )
                    );
            }

        private:
            std::vector<message_type> messages_;
            std::stack<message_type> stocked_;
        };

    } // namespace message
} // namespace rill

#endif /*RILL_MESSAGE_REPORT_HPP*/
