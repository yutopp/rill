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
    class identifier_value;
}


typedef std::string     native_string_t;

class value
{
    typedef std::shared_ptr<literal::identifier_value const>  typed_label_type;

public:
    value();
    value( native_string_t const& simple_typename );

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
        auto get_native_symbol_string() const
            -> native_string_type
        {
            return value_;
        }

    private:
        native_string_type value_;
    };
    typedef std::shared_ptr<symbol_value> symbol_value_ptr;

    inline auto make_symbol(
        symbol_value::native_string_type const& native_symbol_name
        )
        -> symbol_value_ptr
    {
        return std::make_shared<symbol_value>( native_symbol_name );
    }















    // 
    class identifier_value
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

        // test implementetion TODO: remove it
        auto get_native_symbol_string() const
            -> native_string_t
        {
            return simple_name_->get_native_symbol_string();
        }

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
    class simple_identifier_value
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
    // TODO: add overload function that implement template specifierd operator

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


struct parameter_pair
{
    literal::identifier_value_ptr type;
    literal::identifier_value_ptr name;
    value_ptr default_value; // TODO: change to expresison
};

#include <boost/fusion/include/adapt_struct.hpp>
BOOST_FUSION_ADAPT_STRUCT(
    parameter_pair,
    (literal::identifier_value_ptr,   type)
    (literal::identifier_value_ptr, name)
    (value_ptr,                 default_value)
)

inline auto make_parameter_pair(
    literal::identifier_value_ptr const& type,
    literal::identifier_value_ptr const& name = nullptr,
    value_ptr const& default_value = nullptr
    )
    -> parameter_pair
{
    parameter_pair ap = { type, name, default_value };

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