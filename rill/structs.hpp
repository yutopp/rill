#pragma once

#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/enable_shared_from_this.hpp>

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
    typedef boost::shared_ptr<literal::type_value const>  typed_label_type;

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
typedef boost::shared_ptr<value> value_ptr;

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
    typedef boost::shared_ptr<symbol_value> symbol_value_ptr;


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
    typedef boost::shared_ptr<int32_value> int32_value_ptr;
}



// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// expressions
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
class expression
{
public:
    virtual ~expression();

public:
    virtual value_ptr eval( const_environment_ptr const& env ) =0;
};
typedef boost::shared_ptr<expression> expression_ptr;



class term_expression
    : public expression
{
public:
    term_expression( value_ptr const& v );

public:
    value_ptr eval( const_environment_ptr const& env );

private:
    value_ptr value_;
};
typedef boost::shared_ptr<term_expression> term_expression_ptr;



class binary_expression
    : public expression
{
public:
    binary_expression( expression_ptr const& lhs, std::string const& op, expression_ptr const& rhs );

public:
    value_ptr eval( const_environment_ptr const& env );

private:
    expression_ptr lhs_;
    std::string op_;
    expression_ptr rhs_;
};
typedef boost::shared_ptr<binary_expression> binary_expression_ptr;


// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// statements
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
class statement
{
public:
    virtual ~statement();

public:
    virtual void setup_environment( environment_ptr const& ) const =0;
    virtual void eval( const_environment_ptr const& ) const =0;
};
typedef boost::shared_ptr<statement> statement_ptr;



class expression_statement
    : public statement
{
public:
    expression_statement( expression_ptr const& expr );

public:
    void setup_environment( environment_ptr const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

private:
    expression_ptr expression_;
};
typedef boost::shared_ptr<expression_statement> expression_statement_ptr;



class function_definition_statement
    : public statement
{
public:
    function_definition_statement( expression_ptr const& expr );

public:
    void setup_environment( environment_ptr const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

private:
    expression_ptr expression_;
};
typedef boost::shared_ptr<function_definition_statement> function_definition_statement_ptr;



















#include <functional>
typedef std::function<value_ptr(std::vector<value_ptr> const&)> native_function_t;

class native_function_definition_statement
    : public statement
{
public:
    native_function_definition_statement( std::string const& name, native_function_t const& callee );

public:
    void setup_environment( environment_ptr const& ) const;

    void call( std::vector<value_ptr> const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

private:
    std::string name_;
    native_function_t callee_;
};
typedef boost::shared_ptr<function_definition_statement> function_definition_statement_ptr;








typedef std::string symbol;
typedef std::vector<statement_ptr>  program;