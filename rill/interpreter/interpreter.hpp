//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_INTERPRETER_HPP
#define RILL_INTERPRETER_HPP

#include "runner.hpp"

namespace rill
{
    namespace interpreter
    {
        template<typename EnvironmentPtr, typename ActionHolderPtr, typename T>
        void run_on_context( context_ptr const& ctx, EnvironmentPtr const& env, ActionHolderPtr const& holder, T const& node )
        {
            runner r( ctx, holder );

            r.dispatch( node, env );
        }

        template<typename EnvironmentPtr, typename ActionHolderPtr, typename T>
        auto run( EnvironmentPtr const& env, ActionHolderPtr const& holder, T const& node )
            -> std::shared_ptr<runtime>
        {
            // TODO: add global constant initalize phase

            auto const rt = std::make_shared<runtime>();

            run_on_context( rt->create_context(), env, holder, node );
            return rt;
        }
    } // namespace interpreter
} // namespace rill

#endif /*RILL_INTERPRETER_HPP*/