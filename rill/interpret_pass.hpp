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
        // test implementation

        auto const& evaled_lhs = s.lhs_->dispatch( *this, env );
        auto const& evaled_rhs = s.rhs_->dispatch( *this, env );

       /* auto const& ee = nullptr;// env->lookup_env( s.op_ );
        if ( !ee ) {
            // throw
            exit( -2 );
        }*/

        std::vector<value_ptr> v;
        v.push_back( evaled_lhs.value );
        v.push_back( evaled_rhs.value );

        return nullexpr; //std::dynamic_pointer_cast<native_function_definition_statement>( ee->get_stmt() )->callee_( v );
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

    const_environment_ptr operator()( value const& s, environment_ptr const& env ) const
    {
        return nullptr;
    }
};