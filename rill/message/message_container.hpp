//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_MESSAGE_MESSAGE_CONTAINER_HPP
#define RILL_MESSAGE_MESSAGE_CONTAINER_HPP

#include <memory>

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
            using report_pointer = std::shared_ptr<report_type>;
            using const_report_pointer = std::shared_ptr<report_type const>;

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
                -> const_report_pointer
            {
                return report_;
            }

        public:
            template<typename... Args>
            [[noreturn]]
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
            inline auto save_stock_message_pivot() const
                -> void
            {
                report_->append_stocked_message(
                    message_type{ message_level::e_pivot }
                    );
            }

            template<typename... Args>
            inline auto stock_message( Args&&... args ) const
                -> void
            {
                report_->append_stocked_message(
                    message_type{
                        message::message_level::e_note,
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

                if ( m.has_appendix ) {
                    assert( !report_->get_stocked().empty() );
                    for(;;) {
                        auto m = std::move( report_->get_stocked().top() );
                        report_->get_stocked().pop();
                        if ( m.level == message_level::e_pivot ) {
                            break;
                        }

                        static_cast<Derived const*>( this )->message_hook(
                            std::move( m )
                            );
                    }
                }
            }

        protected:
            auto import_messages( const_report_pointer const& rep ) const
                -> void
            {
                report_->import_from( *rep );
            }

        private:
            mutable bool is_error_state_;
            mutable report_pointer report_;
        };

    } // namespace message
} // namespace rill

#endif /*RILL_MESSAGE_MESSAGE_CONTAINER_HPP*/
