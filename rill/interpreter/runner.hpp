//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef RILL_INTERPRETER_RUNNER_HPP
#define RILL_INTERPRETER_RUNNER_HPP

#include <memory>

#include "../tree_visitor_base.hpp"
#include "runtime.hpp"

namespace rill
{
    namespace interpreter
    {
        class runner RILL_CXX11_FINAL
            : public tree_visitor_base
        {
        public:
            runner( context_ptr const&, bool const );

        public:
            // statement_list
            void operator()( statement_list const& ss, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            void operator()( expression_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( return_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( function_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

            void operator()( class_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            // expression
            value_env_pair_t operator()( binary_operator_expression const& e, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            value_env_pair_t operator()( call_expression const& e, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            value_env_pair_t operator()( embedded_function_call_expression const& e, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            value_env_pair_t operator()( term_expression const& e, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            //
            const_environment_ptr operator()( value const& v, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

        private:
            context_ptr context_;
            bool is_on_compile_time_;
        };
    } // namespace interpreter
} // namespace rill

#endif /*RILL_INTERPRETER_RUNNER_HPP*/