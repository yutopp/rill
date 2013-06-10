#pragma once

#include "statement.hpp"

struct result
{
    program product;
};


typedef std::string                     input_type;
typedef input_type::const_iterator      input_iterator;


auto make_syntax_tree( input_type const& source ) -> result;