#include <iostream>

#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

#include <rill/environment.hpp>



    // value's constructor
    value::value( native_string_t const& simple_typename )
        : type_labal_( std::make_shared<literal::single_identifier_value>( 
        std::make_shared<literal::identifier_value>( simple_typename ) ) )
    {}





namespace literal
{




        int32_value::int32_value( int const v )
            : literal_value( "int" )
            , value_( v )
        {}

        int int32_value::get_value() const
        {
            return value_;
        }
}


std::ostream& operator<<( std::ostream& os, value_ptr const& vp )
{
    os << "!debug value output: " << std::endl;

    if ( vp ) {
        if ( vp->is_typed() ) {
            os << "  type  is " << vp->type_labal_->get_last_symbol()->get_native_symbol_string() << std::endl;
            if ( vp->type_labal_->get_last_symbol()->get_native_symbol_string() == "int" ) {
                os << "  value is " << std::dynamic_pointer_cast<literal::int32_value>( vp )->value_ << std::endl;
            } else {
                os << "  value is unknown." << std::endl;
            }
        } else {
            os << "  NOT typed value." << std::endl;
        }
    } else {
        os << "  ERROR: nullptr is setted." << std::endl;
    }

    return os;
}