#ifndef RILL_INTERPRETER_INVOKE_HPP
#define RILL_INTERPRETER_INVOKE_HPP

#include "runner.hpp"

namespace rill
{
    namespace interpreter
    {
        template<typename NodePtr, typename EnvironmentPtr>
        auto run_statement( runner const& r, NodePtr const& node, EnvironmentPtr const& env )
            -> decltype( node->dispatch( r, env ) )
        {
            return node->dispatch( r, env );
        }

        template<typename EnvironmentPtr, typename T>
        void run_with_scope_pop( context_ptr const& ctx, EnvironmentPtr const& env, T const& statements, bool const on_conpile_time = true )
        {
            runner r( ctx, on_conpile_time );

            for( auto const& s : statements )
                run_statement( r, s, env );

            ctx->pop_scope();
        }

        template<typename EnvironmentPtr, typename T>
        void run( EnvironmentPtr const& env, T const& statements, bool const on_conpile_time = true )
        {
            auto const rt = std::make_shared<runtime>();

            run_with_scope_pop( rt->create_context(), env, statements, on_conpile_time );
        }

    } // namespace interpreter
} // namespace rill

#endif /*RILL_INTERPRETER_INVOKE_HPP*/