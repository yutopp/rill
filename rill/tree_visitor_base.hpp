#pragma once

#include <iostream>

#include "environment_fwd.hpp"

//#include "value.hpp"
//#include "expression.hpp"
#include "statement_fwd.hpp"

struct tree_visitor_base
{
public:
    virtual ~tree_visitor_base() {}

public:
    // statement_list
    virtual void operator()( statement_list const& ss, environment_ptr const& env ) const =0;

    // statement
    // virtual void operator()( template_statement const& s, environment_ptr const& env ) const =0;

    virtual void operator()( expression_statement const& s, environment_ptr const& env ) const =0;

    // virtual void operator()( native_function_definition_statement const& s, environment_ptr const& env ) const =0;

    virtual void operator()( class_definition_statement const& s, environment_ptr const& env ) const =0;


    // expression

    // value

public:
    // filter outdated object
    template<typename T>
    void operator()( T const& node, environment_ptr const& env ) const
    {
        std::cerr
            << "DEBUG: message. please implement it!" << std::endl
            << "-> " << typeid(T).name() << std::endl;
    }
};