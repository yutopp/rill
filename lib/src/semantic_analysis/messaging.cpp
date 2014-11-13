//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/semantic_analysis/messaging.hpp>
#include <rill/semantic_analysis/message_code.hpp>

#include <rill/utility/colorize.hpp>


namespace rill
{
    namespace semantic_analysis
    {
        auto messaging::semantic_error(
            boost::filesystem::path const& filepath,
            message_code const& code,
            ast::const_ast_base_ptr const& ast,
            boost::format const& message,
            bool const has_appendix
            ) const
            -> void
        {
            auto const& location
                = ( boost::format( "%1% (l:%2% c:%3%)" )
                    % filepath
                    % ( ast ? ast->line : 999 )
                    % ( ast ? ast->column : 999 )
                    ).str();
            auto const& content
                = message.str();

            send_error( code, location, content, has_appendix );
        }

        auto messaging::save_appendix_information(
            boost::filesystem::path const& filepath,
            message_code const& code,
            ast::const_ast_base_ptr const& ast,
            boost::format const& message
            ) const
            -> void
        {
            auto const& location
                = ( boost::format( "%1% (l:%2% c:%3%)" )
                    % filepath
                    % ( ast ? ast->line : 999 )
                    % ( ast ? ast->column : 999 )
                    ).str();
            auto const& content
                = message.str();

            stock_message( code, location, content );
        }

        auto messaging::message_hook( message_type const& m ) const
            -> void
        {
            // TODO: check if a message is error
            switch( m.level ) {
            case message::message_level::e_normal:
                break;

            case message::message_level::e_warning:
                break;

            case message::message_level::e_error:
                std::cout << ( colorize::standard::fg::red | colorize::standard::bold )
                          << "Error: " << colorize::standard::reset
                          << m.location << std::endl
                          << "  " << colorize::standard::bold
                          << m.content << colorize::standard::reset << std::endl
                          << std::endl;
                break;

            case message::message_level::e_note:
                std::cout << ( colorize::standard::fg::blue | colorize::standard::bold )
                          << "    Note: " << colorize::standard::reset
                          << m.location << std::endl
                          << "      " << colorize::standard::bold
                          << m.content << colorize::standard::reset << std::endl
                          << std::endl;
//                assert( false );
                break;

            default:
                assert( false );
            }
        }

    } // namespace semantic_analysis
} // namespace rill
