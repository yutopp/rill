//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP
#define RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP

#include <memory>

#include "../../tree_visitor_base.hpp"
#include "runtime.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        namespace interpreter
        {
            class runner RILL_CXX11_FINAL
                : public tree_visitor_base
            {
            public:
                runner( context_ptr const& ctx );

            public:
                // statement_list
                void operator()( ast::root const& ss, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

                // statement
                // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

                void operator()( ast::expression_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
                void operator()( ast::return_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
                void operator()( ast::function_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
                // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

                void operator()( ast::class_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

                // expression
                auto operator()( ast::binary_operator_expression const& e, environment_ptr const& env ) const ->environment_ptr RILL_CXX11_OVERRIDE;
                auto operator()( ast::call_expression const& e, environment_ptr const& env ) const ->environment_ptr RILL_CXX11_OVERRIDE;
                auto operator()( ast::embedded_function_call_expression const& e, environment_ptr const& env ) const ->environment_ptr RILL_CXX11_OVERRIDE;
                auto operator()( ast::term_expression const& e, environment_ptr const& env ) const ->environment_ptr RILL_CXX11_OVERRIDE;

                //
                auto operator()( ast::intrinsic_value const& v, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
                auto operator()( ast::variable_value const& v, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;

            private:
                context_ptr context_;
            };
        } // namespace interpreter
    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_COMPILETIME_INTERPRETER_RUNNER_HPP*/