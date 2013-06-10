#pragma once

#include <iostream>

#include "environment_fwd.hpp"

#include "value_fwd.hpp"
#include "expression_fwd.hpp"
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
    virtual value_ptr operator()( term_expression const& s, environment_ptr const& env ) const =0;
    virtual value_ptr operator()( binary_expression const& s, environment_ptr const& env ) const =0;

    // value

public:
    // filter outdated object
    template<typename T>
    nullptr_t operator()( T const& node, environment_ptr const& env ) const
    {
        std::cerr
            << "DEBUG: message. please implement it!" << std::endl
            << "-> " << typeid(T).name() << std::endl;

        return nullptr;
    }
};