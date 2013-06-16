//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>

#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

#include <rill/environment.hpp>



    // value's constructor
    value::value( native_string_t const& simple_typename )
        : intrinsic_typed_identifier_( std::make_shared<literal::single_identifier_value const>( simple_typename ) )
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


std::ostream& operator<<( std::ostream& os, value const& vp )
{
    if ( vp.is_intrinsic_type() ) {
        os << "  type  is " << vp.intrinsic_typed_identifier_->get_base_symbol()->get_native_symbol_string() << std::endl;
        if ( vp.intrinsic_typed_identifier_->get_base_symbol()->get_native_symbol_string() == "int" ) {
            os << "  value is " << dynamic_cast<literal::int32_value const*>( &vp )->value_ << std::endl;
        } else {
            os << "  value is unknown." << std::endl;
        }
    } else {
        os << "  NOT typed value." << std::endl;
    }

    return os;
}


std::ostream& operator<<( std::ostream& os, value_env_pair_t const& v )
{
    os << "!debug value_envid_pair_t output: " << std::endl;
    if ( v.value ) {
        os << *(v.value);
        os << "  Envid is " << ( v.env ? v.env->get_id() : 0 ) << std::endl;

    } else {
        os << "  ERROR: nullptr is setted." << std::endl;
    }
    
    return os;
}