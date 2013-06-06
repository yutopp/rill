#pragma once

#include <vector>
#include <string>
#include <memory>

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


// forward declare
namespace literal
{
    class type_value;
}




class value
{
    typedef std::shared_ptr<literal::type_value const>  typed_label_type;

public:
    value();
    value( std::string const& simple_typename );

    virtual ~value();

public:
    bool is_typed() const;

    auto type() const -> typed_label_type;

private:
    typed_label_type type_labal_;
};
typedef std::shared_ptr<value> value_ptr;

//
class managed_value
{
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

    virtual ~literal_value();
};






namespace literal
{
    class symbol_value
        : public literal_value
    {
    public:
        typedef std::string     native_string_type;

    public:
        symbol_value( native_string_type const& );

    public:


    private:
        native_string_type value_;
    };
    typedef std::shared_ptr<symbol_value> symbol_value_ptr;


    // 
    class type_value
        : public literal_value
    {
    public:
        typedef std::vector<value_ptr>                      template_parameters_type;
        typedef std::unique_ptr<template_parameters_type>   template_parameters_pointer;

    public:
        type_value( bool const is_template_type, symbol_value::native_string_type const& simple_typename );

        virtual ~type_value();

    public:
        bool is_template() const;



        virtual auto template_parameters() const -> template_parameters_pointer;

    private:
        bool is_template_;
        symbol_value_ptr simple_typename_;
    };





    //
    class simple_type_value
        : public type_value
    {
    public:
        simple_type_value( symbol_value::native_string_type const& simple_typename );
    public:


    private:
    };



    //
    class template_type_value
        : public type_value
    {
    public:
        template_type_value( symbol_value::native_string_type const& simple_typename );

    public:


    private:
    };



    class int32_value
        : public literal_value
    {
    public:
        int32_value( int const v );

    public:
        int get_value() const;

    private:
        int value_;
    };
    typedef std::shared_ptr<int32_value> int32_value_ptr;
}