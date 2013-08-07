//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_SEMANTIC_ANALYSIS_LIST_IDENTIFIER_VISITOR_HPP
#define RILL_SEMANTIC_ANALYSIS_LIST_IDENTIFIER_VISITOR_HPP

#include <memory>

#include "../tree_visitor_base.hpp"

namespace rill
{
    namespace semantic_analysis
    {
        //
        //
        //
        class list_identifier_visitor RILL_CXX11_FINAL
            : public tree_visitor_base
        {
        public:
            // statement_list
            void operator()( statement_list const& ss, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            void operator()( expression_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( return_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( function_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( class_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            // expression
            auto operator()( binary_operator_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( call_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( embedded_function_call_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( term_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;

            //
            auto operator()( intrinsic_value const& v, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( variable_value const& s, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
        };


        //
        //
        //
        class determine_parameter_signature_visitor RILL_CXX11_FINAL
            : public tree_visitor_base
        {
        public:
            determine_parameter_signature_visitor()
                : solved_num_( 0 )
            {}

        public:
            // statement_list
            void operator()( statement_list const& ss, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            // statement
            // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

            void operator()( expression_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( return_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( function_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;
            void operator()( class_definition_statement const& s, environment_ptr const& env ) const RILL_CXX11_OVERRIDE;

            // expression
            auto operator()( binary_operator_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( call_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( embedded_function_call_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( term_expression const& e, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;

            //
            auto operator()( intrinsic_value const& v, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;
            auto operator()( variable_value const& s, environment_ptr const& env ) const -> environment_ptr RILL_CXX11_OVERRIDE;

        public:
            auto get_solved_num() const -> std::size_t
            {
                return solved_num_;
            }

        private:
            std::size_t solved_num_;
        };

    } // namespace semantic_analysis
} // namespace rill

#endif /*RILL_SEMANTIC_ANALYSIS_LIST_IDENTIFIER_VISITOR_HPP*/