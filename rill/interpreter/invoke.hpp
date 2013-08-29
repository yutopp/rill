//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_INTERPRETER_INVOKE_HPP
#define RILL_INTERPRETER_INVOKE_HPP

#include "runner.hpp"

namespace rill
{
    namespace interpreter
    {
#if 0
        template<typename NodePtr, typename EnvironmentPtr>
        auto run_statement( runner const& r, NodePtr const& node, EnvironmentPtr const& env )
            -> decltype( node->dispatch( r, env ) )
        {
            return node->dispatch( r, env );
        }

        template<typename EnvironmentPtr, typename T>
        void run_on_context( context_ptr const& ctx, EnvironmentPtr const& env, T const& statements, bool const on_conpile_time = true )
        {
            runner r( ctx, on_conpile_time );

            for( auto const& s : statements )
                run_statement( r, s, env );
        }

        template<typename EnvironmentPtr, typename T>
        auto run( EnvironmentPtr const& env, T const& statements, bool const on_conpile_time = true )
            -> std::shared_ptr<runtime>
        {
            auto const rt = std::make_shared<runtime>();

            run_on_context( rt->create_context(), env, statements, on_conpile_time );
            return rt;
        }
#endif
    } // namespace interpreter
} // namespace rill

#endif /*RILL_INTERPRETER_INVOKE_HPP*/