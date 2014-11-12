//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_MESSAGE_MESSAGE_CONTAINER_HPP
#define RILL_MESSAGE_MESSAGE_CONTAINER_HPP

#include "report.hpp"
#include "message.hpp"


namespace rill
{
    namespace message
    {
        template<typename MessageT, typename Derived>
        class message_container
        {
        public:
            using message_type = MessageT;
            using report_type = report<message_type>;

        public:
            message_container()
                : is_error_state_( false )
                , report_( std::make_shared<report_type>() )
            {}

        public:
            inline auto is_error_state() const
                -> bool
            {
                return is_error_state_;
            }

            inline auto is_error_state( bool const b ) const
                -> void
            {
                is_error_state_ = b;
            }

        public:
            inline auto get_report() const
                -> std::shared_ptr<report_type const>
            {
                return report_;
            }

        public:
            template<typename... Args>
            auto send_error( Args&&... args ) const
                -> void
            {
                is_error_state_ = true;

                throw message_type{
                    message::message_level::e_error,
                    std::forward<Args>( args )...
                    };
            }

            template<typename... Args>
            auto send_warning( Args&&... args ) const
                -> void
            {
                save_message(
                    message_type{
                        message::message_level::e_warning,
                        std::forward<Args>( args )...
                        }
                    );
            }

            template<typename... Args>
            auto send_message( Args&&... args ) const
                -> void
            {
                save_message(
                    message_type{
                        message::message_level::e_normal,
                        std::forward<Args>( args )...
                        }
                    );
                }

        protected:
            template<typename T>
            auto save_message( T&& m ) const
                -> void
            {
                static_cast<Derived const*>( this )->message_hook( m );
                report_->append_message( std::forward<T>( m ) );
            }

        private:
            mutable bool is_error_state_;
            mutable std::shared_ptr<report_type> report_;
        };

    } // namespace message
} // namespace rill

#endif /*RILL_MESSAGE_MESSAGE_CONTAINER_HPP*/
