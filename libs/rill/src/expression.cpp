#include <iostream>

#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

#include <rill/environment.hpp>




// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// expressions
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------

    expression::~expression() {}



    term_expression::term_expression( value_ptr const& v )
        : value_( v )
    {}

    value_ptr term_expression::eval( const_environment_ptr const& env )
    {
        return value_;
    }



    binary_expression::binary_expression( expression_ptr const& lhs, literal::identifier_value_ptr const& op, expression_ptr const& rhs )
        : lhs_( lhs )
        , op_( op )
        , rhs_( rhs )
    {}

    value_ptr binary_expression::eval( const_environment_ptr const& env )
    {
        auto const& evaled_lhs = lhs_->eval( env );
        auto const& evaled_rhs = rhs_->eval( env );

        auto const& ee = env->lookup_env( op_->get_native_symbol_string() );
        if ( !ee ) {
            // throw
            exit( -2 );
        }

        std::vector<value_ptr> v;
        v.push_back( evaled_lhs );
        v.push_back( evaled_rhs );
//        std::dynamic_pointer_cast<native_function_definition_statement>( ee->get_stmt() )->call( v );


        return std::make_shared<value>( /**lhs_->eval() + *rhs_->eval()*/ );
    }
