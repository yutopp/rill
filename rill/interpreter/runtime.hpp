//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_INTERPRETER_RUNTIME_HPP
#define RILL_INTERPRETER_RUNTIME_HPP

#include <cassert>
#include <stack>
#include <memory>
#include <rill/value_fwd.hpp>

namespace rill
{
    namespace interpreter
    {
        class runtime;
        typedef std::shared_ptr<runtime> runtime_ptr;


        class scope
        {
        public:
            scope() {}
            scope( argument_list const& args )
                : args_( args )
            {}

        public:
            void push_value( const_value_ptr const& val )
            {
            };

            auto get_args() const
                -> argument_list const&
            {
                return args_;
            }

            void set_return_value( value_env_pair_t const& v )
            {
                return_value_ = v;
            }

            auto get_return_value() const
                -> value_env_pair_t const&
            {
                return return_value_;
            }

        private:
            argument_list args_;
            value_env_pair_t return_value_;
        };
        typedef std::shared_ptr<scope>          scope_ptr;
        typedef std::shared_ptr<scope const>    const_scope_ptr;

        class context
        {
        public:
            context()
            {
                push_scope();
            }

        public:
            auto current_scope()
                -> scope_ptr
            {
                return scope_stack_.top();
            }

            auto current_scope() const
                -> const_scope_ptr
            {
                return scope_stack_.top();
            }

            auto push_scope()
                -> scope_ptr
            {
                auto const s = std::make_shared<scope>();
                scope_stack_.push( s );

                return s;
            }

            auto push_entry_scope( argument_list const& args )
                -> scope_ptr
            {
                auto const s = std::make_shared<scope>( args );
                scope_stack_.push( s );

                return s;
            }

            void pop_scope()
            {
                auto const s = current_scope();
                assert( s != nullptr );

                // s->unwind();
                scope_stack_.pop();
            }

        private:
            std::weak_ptr<runtime> runtime_;
            std::stack<scope_ptr> scope_stack_;
        };
        typedef std::shared_ptr<context> context_ptr;

        class runtime
        {
        public:
            auto create_context()
                -> context_ptr
            {
                return std::make_shared<context>();
            }

        private:

        };
        

    } // namespace interpreter
} // namespace rill

#endif /*RILL_INTERPRETER_RUNTIME_HPP*/