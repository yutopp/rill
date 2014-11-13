//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_MESSAGING_HPP
#define RILL_SEMANTIC_ANALYSIS_MESSAGING_HPP

#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>

#include "../message/message_container.hpp"
#include "message_code_fwd.hpp"

#include "../ast/ast_base.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        class messaging
            : public message::message_container<
                message::message_object<message_code>,
                messaging
                >
        {
        public:
            [[noreturn]]
            auto semantic_error(
                boost::filesystem::path const& filepath,
                message_code const& code,
                ast::const_ast_base_ptr const& ast,
                boost::format const& message,
                bool const has_appendix = false
                ) const
                -> void;

            auto save_appendix_information(
                boost::filesystem::path const& filepath,
                message_code const& code,
                ast::const_ast_base_ptr const& ast,
                boost::format const& message
                ) const
                -> void;

            auto message_hook( message_type const& m ) const
                -> void;
        };

        template<typename S>
        auto format( S&& s )
        {
            // TODO: support Boost.Locale
            return boost::format( std::forward<S>( s ) );
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_MESSAGING_HPP*/
