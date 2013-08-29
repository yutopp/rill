//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <memory>
#include <vector>
// for helper
#include <iostream>

#include "detail/specifier.hpp"


namespace rill
{
    namespace ast
    {
        RILL_AST_FWD_DECL( value, value )

        typedef std::vector<const_value_ptr>    argument_list;
        typedef std::shared_ptr<argument_list>  argument_list_ptr;

        typedef std::vector<value_ptr>                      template_argument_list;
        typedef std::shared_ptr<template_argument_list>     template_argument_list_ptr;


        RILL_AST_FWD_DECL_IN_NAMESPACE( intrinsic, value_base, value )

        //
        RILL_AST_FWD_DECL_IN_NAMESPACE( intrinsic, single_identifier_value_base, value )

        //
        RILL_AST_FWD_DECL_IN_NAMESPACE( intrinsic, identifier_value, value )

        // 
        RILL_AST_FWD_DECL_IN_NAMESPACE( intrinsic, single_identifier_value, value )

        /*
        //
        struct single_template_identifier_value;
        typedef std::shared_ptr<single_template_identifier_value> single_template_identifier_value_ptr;
        */

        RILL_AST_FWD_DECL_IN_NAMESPACE( intrinsic, symbol_value, value )

        RILL_AST_FWD_DECL_IN_NAMESPACE( intrinsic, int32_value, value )


        //
        //
        RILL_AST_FWD_DECL( intrinsic_value, value )

        //
        //
        RILL_AST_FWD_DECL( variable_value, value )


        //
        std::ostream& operator<<( std::ostream& os, value const& vp );

    } // namespace ast
} // namespace rill
