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
#include <map>
#include <unordered_map>

#include <rill/ast/value_fwd.hpp>

namespace rill
{
    namespace interpreter
    {
        class runtime;
        typedef std::shared_ptr<runtime> runtime_ptr;

        typedef std::map<environment_id_t, ast::const_value_ptr> value_table_t;



        class scope
        {
        public:
            scope() {}

        public:

        private:
        };
        typedef std::shared_ptr<scope>          scope_ptr;
        typedef std::shared_ptr<scope const>    const_scope_ptr;




        enum struct stack_state
        {
            none
        };



        class value_wrapper
        {
        public:
            value_wrapper( ast::value_ptr const& vp, stack_state const& s = stack_state::none )
                : value( vp )
                , state( s )
            {}

            ast::value_ptr value;
            stack_state state;
        };





        //
        // runtime context
        //
        class context
        {
        public:
            context()
            {
                push_new_scope();
            }

        public:
            auto push_value( ast::value_ptr const& v )
                -> void
            {
                value_stack_.emplace( v );
                //std::cout << "Stack << " << *v << std::endl;
            }

            auto pop_value()
                -> value_wrapper
            {
                auto const p = value_stack_.top();
                value_stack_.pop();

                //std::cout << "Stack >> " << *p.value << std::endl;
                return p;
            }

            auto construct_variable( environment_id_t const& env_id, ast::value_ptr const& val )
                -> void
            {
                variable_map_[env_id] = val;
            }

            auto get_variable_value_by_id( environment_id_t const& env_id )
                -> ast::value_ptr
            {
                return variable_map_.at( env_id );
            }

            auto push_new_scope()
                -> scope_ptr
            {
                auto const& sc = std::make_shared<scope>();

                scope_stack_.emplace( sc );
                return sc;
            }

            auto pop_scope()
                -> void
            {
                scope_stack_.pop();
            }

            auto current_scope()
                -> scope_ptr
            {
                return scope_stack_.top();
            }

            auto current_stack_value()
                -> ast::value_ptr
            {
                return value_stack_.top().value;
            }

        private:
            std::weak_ptr<runtime> runtime_;

            std::stack<value_wrapper> value_stack_;
            std::unordered_map<environment_id_t, ast::value_ptr> variable_map_;

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