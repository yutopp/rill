//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_AST_DETAIL_VISITOR_REPORT_HPP
#define RILL_AST_DETAIL_VISITOR_REPORT_HPP

#include <vector>
#include <algorithm>

#include "../../message/message.hpp"


namespace rill
{
    namespace ast
    {
        namespace detail
        {
            class visitor_report
            {
            public:
                inline auto is_errored() const
                    -> bool
                {
                    return std::count_if( messages_.cbegin(), messages_.cend(), []( auto&& v ) {
                        return v.level == message::message_level::e_error;
                    }) > 0;
                }

                inline auto append_message( message::message_object&& msg )
                    -> void
                {
                    messages_.emplace_back( std::move( msg ) );
                }

                inline auto get_messages() const
                    -> std::vector<message::message_object> const&
                {
                    return messages_;
                }

            private:
                std::vector<message::message_object> messages_;
            };

        } // namespace detail
    } // namespace ast
} // namespace rill

#endif /*RILL_AST_DETAIL_VISITOR_REPORT_HPP*/
