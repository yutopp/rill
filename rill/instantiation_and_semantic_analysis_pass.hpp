#pragma once

#include "tree_visitor_base.hpp"

#include "value.hpp"
#include "statement.hpp"
#include "expression.hpp"

struct instantiation_and_semantic_analysis_pass
    : public tree_visitor_base
{
    void operator()( statement_list const& ss, environment_ptr const& env ) const
    {
        for( auto const& s : ss )
            s->dispatch( *this, env );
    }

    void operator()( expression_statement const& s, environment_ptr const& env ) const
    {
        // nothing to do...
    }
    void operator()( return_statement const& s, environment_ptr const& env ) const
    {
    }
    void operator()( function_definition_statement const& s, environment_ptr const& env ) const
    {
    }
    // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

    void operator()( class_definition_statement const& s, environment_ptr const& env ) const
    {
        //env->add_class( s );
    }

    //
    value_env_pair_t operator()( binary_operator_expression const& s, environment_ptr const& env ) const
    {
        return nullexpr;
    }

    value_env_pair_t operator()( call_expression const& s, environment_ptr const& env ) const
    {
        return nullexpr;
    }

    value_env_pair_t operator()( embedded_function_call_expression const& s, environment_ptr const& env ) const
    {
        return nullexpr;
    }

    value_env_pair_t operator()( term_expression const& s, environment_ptr const& env ) const
    {
        return nullexpr;
    }

    //
    const_environment_ptr operator()( value const& s, environment_ptr const& env ) const
    {
        return nullptr;
    }
};