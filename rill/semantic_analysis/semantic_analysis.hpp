//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_HPP
#define RILL_SEMANTIC_ANALYSIS_HPP

#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>

#include "identifier_collector.hpp"
#include "analyzer.hpp"


namespace rill
{
    namespace semantic_analysis
    {
        //
        template<
            typename Node,
            typename EnvPtr = environment_base_ptr
            >
        auto collect_identifier(
            global_environment_ptr const& g_env,
            Node const& node,
            EnvPtr const& env = nullptr,
            boost::filesystem::path const& base_path = boost::filesystem::path()
            )
        {
            identifier_collector visitor( g_env, base_path );
            visitor.dispatch( node, env );

            return visitor.get_report();
        }

        //
        template<
            typename Node,
            typename ActionHolderPtr,
            typename EnvPtr = environment_base_ptr
            >
        auto analyse_and_complement(
            global_environment_ptr const& g_env,
            Node const& node,
            ActionHolderPtr const& action_holder,
            analyzer_options const& options = analyzer_options{},
            abstract_system_info const& sysinfo = abstract_system_info{},
            EnvPtr const& env = nullptr
            )
        {
            analyzer visitor( g_env, action_holder, options, sysinfo );
            visitor.dispatch( node, env );

            return visitor.get_report();
        }

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_HPP*/
