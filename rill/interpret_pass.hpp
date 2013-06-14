#pragma once

#include "tree_visitor_base.hpp"

#include "value.hpp"
#include "statement.hpp"
#include "expression.hpp"

struct runtime_interpret_tag;

template<typename Tag>
struct interpret_pass;

// runtime 
template<>
struct interpret_pass<runtime_interpret_tag>
    : public tree_visitor_base
{
    void operator()( statement_list const& ss, environment_ptr const& env ) const
    {
        for( auto const& s : ss )
            s->dispatch( *this, env );
    }

    void operator()( expression_statement const& s, environment_ptr const& env ) const
    {
        std::cout
            << "in expression_statement dispach of interpret_pass<runtime_interpret_tag>" << std::endl
            << s.expression_->dispatch( *this, env ) << std::endl;
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
    value_ptr operator()( binary_operator_expression const& s, environment_ptr const& env ) const
    {
        // test implementation

        auto const& evaled_lhs = s.lhs_->dispatch( *this, env );
        auto const& evaled_rhs = s.rhs_->dispatch( *this, env );

        auto const& ee = env->lookup_env( s.op_ );
        if ( !ee ) {
            // throw
            exit( -2 );
        }

        std::vector<value_ptr> v;
        v.push_back( evaled_lhs );
        v.push_back( evaled_rhs );

        return std::dynamic_pointer_cast<native_function_definition_statement>( ee->get_stmt() )->callee_( v );
    }

    value_ptr operator()( call_expression const& s, environment_ptr const& env ) const
    {
        return nullptr;
    }

    value_ptr operator()( embedded_function_call_expression const& s, environment_ptr const& env ) const
    {
        return nullptr;
    }

    value_ptr operator()( term_expression const& s, environment_ptr const& env ) const
    {
        return s.value_ /* todo call dispatch */;
    }
};