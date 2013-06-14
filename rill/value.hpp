#pragma once

#include <vector>
#include <string>
#include "value_fwd.hpp"

#include "environment_fwd.hpp"


// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// values
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
enum struct value_spec
{
    constatnt = 0
};



typedef std::string     native_string_t;



struct value
{
    typedef std::shared_ptr<literal::identifier_value const>  typed_label_type;

public:
    value() {}
    value( native_string_t const& simple_typename );

    virtual ~value() {}

public:
    bool is_typed() const
    {
        return type_labal_.use_count() != 0;
    }

public:
    typed_label_type type_labal_;
};








//
class literal_value
    : public value
{
public:
    literal_value()
    {}

    literal_value( std::string const& name )
        : value( name )
    {}

    virtual ~literal_value()
    {};
};






namespace literal
{
    struct symbol_value
        : public literal_value
    {
    public:
        typedef std::string     native_string_type;

    public:
        symbol_value( native_string_type const& name )
            : value_( name )
        {}

    public:
        auto get_native_symbol_string() const
            -> native_string_type
        {
            return value_;
        }

    private:
        native_string_type value_;
    };

    inline auto make_symbol(
        symbol_value::native_string_type const& native_symbol_name
        )
        -> symbol_value_ptr
    {
        return std::make_shared<symbol_value>( native_symbol_name );
    }















    // 
    struct identifier_value
        : public literal_value
    {
    public:
        typedef std::vector<value_ptr>                      template_parameters_type;
        typedef std::unique_ptr<template_parameters_type>   template_parameters_pointer;

    public:
        identifier_value( bool const is_template_type, symbol_value::native_string_type const& simple_typename );

        virtual ~identifier_value();

    public:
        bool is_template() const;

        virtual auto template_parameters() const -> template_parameters_pointer;

        //
        auto get_last_symbol() const
            -> symbol_value_ptr
        {
            return simple_name_;
        }

    private:
        bool is_template_;
        symbol_value_ptr simple_name_; // TODO: support namespace and so on
    };
    typedef std::shared_ptr<identifier_value> identifier_value_ptr;








    //
    struct simple_identifier_value
        : public identifier_value
    {
    public:
        // TODO: support namespace and so on
        simple_identifier_value( native_string_t const& simple_typename );
    public:


    private:
    };
    typedef std::shared_ptr<simple_identifier_value> simple_identifier_value_ptr;


    inline auto make_simple_identifier( native_string_t const& simple_typename )
        -> simple_identifier_value_ptr
    {
        return std::make_shared<simple_identifier_value>( simple_typename );
    }


    inline auto make_binary_operator_identifier(
        symbol_value_ptr const& symbol_name
        )
        -> simple_identifier_value_ptr
    {
        return make_simple_identifier( "%binary%operator_" + symbol_name->get_native_symbol_string() );
    }
    // TODO: add overload function that implement template specified operator

    inline auto make_binary_operator_symbol(
        symbol_value_ptr const& symbol_name
        )
        -> symbol_value_ptr
    {
        return make_symbol( "%binary%operator_" + symbol_name->get_native_symbol_string() );
    }




/*

    //
    class template_identifier_value
        : public identifier_value
    {
    public:
        template_identifier_value( symbol_value::native_string_type const& simple_typename );

    public:


    private:
    };*/



    struct int32_value
        : public literal_value
    {
    public:
        int32_value( int const v );

    public:
        int get_value() const;

    public:
        int const value_;
    };
    typedef std::shared_ptr<int32_value> int32_value_ptr;
}


struct parameter_pair
{
    literal::identifier_value_ptr name;
    literal::identifier_value_ptr type;
    value_ptr default_value; // TODO: change to expresison
};

#include <boost/fusion/include/adapt_struct.hpp>
BOOST_FUSION_ADAPT_STRUCT(
    parameter_pair,
    (literal::identifier_value_ptr, name)
    (literal::identifier_value_ptr, type)
    (value_ptr,                     default_value)
)

inline auto make_parameter_pair(
    literal::identifier_value_ptr const& name,
    literal::identifier_value_ptr const& type,
    value_ptr const& default_value = nullptr
    )
    -> parameter_pair
{
    parameter_pair ap = { name, type, default_value };

    return ap;
}

inline auto make_parameter_pair(
    literal::identifier_value_ptr const& type,
    value_ptr const& default_value = nullptr
    )
    -> parameter_pair
{
    parameter_pair ap = { nullptr, type, default_value };

    return ap;
}


typedef std::vector<parameter_pair> parameter_list;

// test imprementation
inline auto make_parameter_list(
    parameter_pair const& pp
    )
    -> parameter_list
{
    parameter_list pl;
    pl.push_back( pp ); // test code

    return pl;
}






#include <iostream>
std::ostream& operator<<( std::ostream& os, value_ptr const& vp );