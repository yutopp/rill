//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment/environment.hpp>


namespace rill
{
    kind::type_value const function_symbol_environment::KindValue
        = kind::type_value::e_function;


    auto function_symbol_environment::append_parameter_variable(
        variable_symbol_environment_ptr const& v_env
        )
        -> void
    {
        parameter_decl_ids_.push_back( v_env->get_id() );

        // memo parameter variable types
        parameter_type_ids_.push_back( v_env->get_type_id() );
    }

} // namespace rill
