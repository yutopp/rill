#include <iostream>

#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

#include <rill/environment.hpp>



    // value's constructor
    value::value( native_string_t const& simple_typename )
        : type_labal_( std::make_shared<literal::simple_identifier_value>( simple_typename ) )
    {}





namespace literal
{




    // 
        identifier_value::identifier_value( bool const is_template_type, native_string_t const& simple_typename )
            : is_template_( is_template_type )
            , simple_name_( std::make_shared<symbol_value>( simple_typename ) )
        {}

        identifier_value::~identifier_value() {}

        bool identifier_value::is_template() const
        {
            return is_template_;
        }


        auto identifier_value::template_parameters() const -> template_parameters_pointer
        {
            return nullptr;
        }




    //
        simple_identifier_value::simple_identifier_value( native_string_t const& simple_typename )
            : identifier_value( false, simple_typename )
        {}



/*
    //
        template_type_value::template_type_value( symbol_value::native_string_type const& simple_typename )
            : type_value( true, simple_typename )
        {}
*/



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