#include <iostream>

#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

#include <rill/environment.hpp>

    //
    value::value()
    {}

    value::value( std::string const& simple_typename )
        : type_labal_( std::make_shared<literal::simple_identifier_value>( simple_typename ) )
    {}

    value::~value() {}

    bool value::is_typed() const
    {
        return !type_labal_.use_count() != 0;
    }

    auto value::type() const -> typed_label_type
    {
        return type_labal_;
    }




//
literal_value::~literal_value() {}






namespace literal
{
        symbol_value::symbol_value( native_string_type const& )
        {
        }



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
