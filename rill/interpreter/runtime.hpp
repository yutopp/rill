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

#include <boost/optional.hpp>

#include "../ast/value_fwd.hpp"
#include "../environment_fwd.hpp"


namespace rill
{
    namespace interpreter
    {
        class runtime;
        typedef std::shared_ptr<runtime> runtime_ptr;

        typedef std::map<environment_id_t, ast::const_value_ptr> value_table_t;
        typedef std::pair<environment_id_t, ast::value_ptr> env_value_pair_t;

        namespace variable_option
        {
            struct normal_tag {};
            auto const normal_k = normal_tag();

            struct parameter_tag {};
            auto const parameter_k = parameter_tag();
        }


        class variable_storage
        {
        public:
            typedef ast::value_ptr      value_type;

        public:
            auto bind( environment_id_t const& id, value_type const& val )
                -> void
            {
                storage_[id] = val;
            }

            auto unbind( environment_id_t const& id )
                -> void
            {
                storage_.erase( id );
            }

            auto at( environment_id_t const& id )
                -> value_type
            {
                return storage_.at( id );
            }

        private:
            // variable environment id -> storage
            std::unordered_map<environment_id_t, value_type> storage_;
        };
        typedef std::shared_ptr<variable_storage>   variable_storage_ptr;




        class scope
        {
        public:
            scope( variable_storage_ptr const& vs )
                : variable_storage_on_context_( vs )
            {}

        public:
            void set_return_state( environment_id_t const& env_id, ast::value_ptr const& value )
            {
                return_value_holder_ = std::make_pair( env_id, value );
            }

            auto get_return_status()
                -> boost::optional<env_value_pair_t> const&
            {
                return return_value_holder_;
            }

            bool is_scope_finished() const
            {
                return return_value_holder_ != boost::none;
            }

            auto add_parameter_variable_env_id( environment_id_t const& env_id )
                -> void
            {
                parameter_variable_env_list_.push_back( env_id );
            }

            auto get_parameter_variable()
                -> std::vector<variable_storage::value_type>
            {
                std::vector<variable_storage::value_type> r;
                for( auto const& id : parameter_variable_env_list_ )
                    r.push_back( variable_storage_on_context_->at( id ) );

                return r;
            }

        public:
            auto push_value( variable_storage::value_type const& v )
                -> void
            {
                value_stack_.emplace( v );
                std::cout << "Stack << " << *v << std::endl;
            }

            auto pop_value()
                -> variable_storage::value_type
            {
                auto const v = value_stack_.top();
                value_stack_.pop();

                std::cout << "Stack >> " << *v << std::endl;
                return v;
            }

            auto current_stack_value()
                -> variable_storage::value_type
            {
                return value_stack_.top();
            }

        private:
            variable_storage_ptr variable_storage_on_context_;
            std::stack<variable_storage::value_type> value_stack_;

            std::vector<environment_id_t> parameter_variable_env_list_;
            boost::optional<env_value_pair_t> return_value_holder_;
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

            ast::value_ptr value; // TODO: change to another structure
            stack_state state;
        };





        //
        // runtime context
        //
        class context
        {
        public:
            context()
                : variable_storage_( std::make_shared<variable_storage>() )
            {
                push_new_scope();
            }

        public:
            auto push_new_scope()
                -> scope_ptr
            {
                auto const& sc = std::make_shared<scope>( variable_storage_ );

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

        public:
            auto construct_variable( variable_option::normal_tag, environment_id_t const& env_id, ast::value_ptr const& val )
                -> void
            {
                variable_storage_->bind( env_id, val );
            }

            auto construct_variable( variable_option::parameter_tag, environment_id_t const& env_id, ast::value_ptr const& val )
                -> void
            {
                variable_storage_->bind( env_id, val );
                current_scope()->add_parameter_variable_env_id( env_id );
            }

            auto get_variable_value_by_id( environment_id_t const& env_id )
                -> ast::value_ptr
            {
                return variable_storage_->at( env_id );
            }

        public:
            template<typename ...Args>
            auto push_value( Args&&... xs )
                -> void
            {
                current_scope()->push_value( std::forward<Args>( xs )... );
            }

            auto pop_value()
                -> decltype( current_scope()->pop_value() )
            {
                return current_scope()->pop_value();
            }

            auto current_stack_value()
                -> decltype( current_scope()->current_stack_value() )
            {
                return current_scope()->current_stack_value();
            }

        private:
            std::weak_ptr<runtime> runtime_;
            variable_storage_ptr variable_storage_;

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
