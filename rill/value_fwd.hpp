//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include <memory>
#include <vector>

struct value;
typedef std::shared_ptr<value>          value_ptr;
typedef std::shared_ptr<value const>    const_value_ptr;

typedef std::vector<const_value_ptr>    argument_list;
typedef std::shared_ptr<argument_list>  argument_list_ptr;

typedef std::vector<value_ptr>                      template_argument_list;
typedef std::shared_ptr<template_argument_list>     template_argument_list_ptr;


//
class literal_value;






namespace literal
{
    struct symbol_value;
    typedef std::shared_ptr<symbol_value> symbol_value_ptr;








    //
    struct single_identifier_value_base;
    typedef std::shared_ptr<single_identifier_value_base>       single_identifier_value_base_ptr;
    typedef std::shared_ptr<single_identifier_value_base const> const_single_identifier_value_base_ptr;

    //
    struct identifier_value;
    typedef std::shared_ptr<identifier_value>       identifier_value_ptr;
    typedef std::shared_ptr<identifier_value const> const_identifier_value_ptr;




    // 
    class single_identifier_value;
    typedef std::shared_ptr<single_identifier_value>        single_identifier_value_ptr;
    typedef std::shared_ptr<single_identifier_value const>  const_single_identifier_value_ptr;

/*
    //
    struct single_template_identifier_value;
    typedef std::shared_ptr<single_template_identifier_value> single_template_identifier_value_ptr;
*/



    struct int32_value;
    typedef std::shared_ptr<int32_value> int32_value_ptr;
}

#include <iostream>
std::ostream& operator<<( std::ostream& os, value const& vp );
