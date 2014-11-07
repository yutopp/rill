//[
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_ENVIRONMENT_FWD_HPP
#define RILL_ENVIRONMENT_ENVIRONMENT_FWD_HPP

#include <memory>
#include <vector>
#include <limits>
#include <functional>

#include <boost/strong_typedef.hpp>

#include "../config/macros.hpp"
#include "global_environment_fwd.hpp"
#include "module_id.hpp"


namespace rill
{
    BOOST_STRONG_TYPEDEF( std::size_t, environment_id_t )
    environment_id_t const environment_id_limit = environment_id_t( std::numeric_limits<std::size_t>::max() - 1 );
    environment_id_t const environment_id_undefined = environment_id_t( std::numeric_limits<std::size_t>::max() );

    typedef std::vector<environment_id_t> environment_id_list_t;

    //
    struct root_initialize_tag {};

    class environment_unit;
    using environment_unit_ptr = std::shared_ptr<environment_unit>;
    using const_environment_unit_ptr = std::shared_ptr<environment_unit const>;
    using weak_environment_unit_ptr = std::weak_ptr<environment_unit>;
    using const_weak_environment_unit_ptr = std::weak_ptr<environment_unit const>;

    // forward decleration
    class environment_base;

    typedef std::shared_ptr<environment_base>        environment_base_ptr;
    typedef std::shared_ptr<environment_base const>  const_environment_base_ptr;
    typedef std::weak_ptr<environment_base>          weak_environment_base_ptr;
    typedef std::weak_ptr<environment_base const>    const_weak_environment_base_ptr;

    typedef std::vector<const_environment_base_ptr>  type_environment_base_ptr_list;


    //
    struct environment_parameter_t
    {
        weak_global_environment_ptr global_env;
        environment_id_t id;
        weak_environment_unit_ptr parent;
        module_id_t mod_id;
        bool is_private;
    };

    //
    class multiple_set_environment;
    typedef std::shared_ptr<multiple_set_environment> multiple_set_environment_ptr;
    typedef std::shared_ptr<multiple_set_environment const> const_multiple_set_environment_ptr;



    //
    class single_identifier_environment_base;
    typedef std::shared_ptr<single_identifier_environment_base> single_identifier_environment_base_ptr;
    typedef std::shared_ptr<single_identifier_environment_base const> const_single_identifier_environment_base_ptr;

    //
    class root_environment;

    class namespace_envitronment;
    class alias_environment;


    class has_parameter_environment_base;
    typedef std::shared_ptr<has_parameter_environment_base> has_parameter_environment_base_ptr;
    typedef std::shared_ptr<has_parameter_environment_base const> const_has_parameter_environment_base_ptr;


    //
    template<typename>
    class has_parameter_environment;


    //
    class template_set_environment;
    typedef std::shared_ptr<template_set_environment>           template_set_environment_ptr;


    //
    class template_environment;
    typedef std::shared_ptr<template_environment>               template_environment_ptr;


    //
    class function_symbol_environment;
    typedef std::shared_ptr<function_symbol_environment>        function_symbol_environment_ptr;
    typedef std::shared_ptr<function_symbol_environment const>  const_function_symbol_environment_ptr;


    //
    class variable_symbol_environment;
    typedef std::shared_ptr<variable_symbol_environment>        variable_symbol_environment_ptr;
    typedef std::shared_ptr<variable_symbol_environment const>  const_variable_symbol_environment_ptr;


    //
    class class_symbol_environment;
    typedef std::shared_ptr<class_symbol_environment>           class_symbol_environment_ptr;
    typedef std::shared_ptr<class_symbol_environment const>     const_class_symbol_environment_ptr;


    //
    std::ostream& operator<<( std::ostream& os, const_environment_base_ptr const& env );
} // namespace rill


namespace std
{
    template<>
    struct hash<rill::environment_id_t>
    {
    public:
        auto operator()( rill::environment_id_t const& t ) const
            -> std::size_t
        {
            return std::hash<std::size_t>()( t.t );
        }
    };
}

#endif /*RILL_ENVIRONMENT_ENVIRONMENT_FWD_HPP*/
