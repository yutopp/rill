//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <cassert>
#include <memory>
#include <unordered_map>
#include <bitset>
#include <vector>
#include <utility>
#include <boost/range/adaptor/transformed.hpp>

#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "has_parameter_environment_base.hpp"
#include "template_environment.hpp"

namespace rill
{
    //
    // for template instantiation
    //
    class template_set_environment RILL_CXX11_FINAL
        : public has_parameter_environment_base
    {
    public:
        template_set_environment( environment_parameter_t&& pp )
            : has_parameter_environment_base( std::move( pp ) )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_template_set;
        }

        auto get_inner_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_template;
        }

        template<typename... Args>
        auto allocate_inner_env( Args&&... args )
            -> template_environment_ptr
        {
            // NOTE: parant environment is not "this env" but "one rank top env"
            assert( has_parent() );
            return get_parent_env()->template allocate_env<template_environment>( get_id(), std::forward<Args>( args )... );
        }

        // delegate lookup
        auto lookup( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->lookup( name ); }
        auto lookup( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->lookup( name ); }

        auto find_on_env( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->find_on_env( name ); }
        auto find_on_env( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->find_on_env( name ); }

    private:
        type_id_list_t template_parameter_type_ids_;
    };

} // namespace rill
