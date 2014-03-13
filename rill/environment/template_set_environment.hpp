//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <memory>
#include <map>
#include <utility>

#include "has_parameter_environment_base.hpp"
#include "template_environment.hpp"


namespace rill
{
    // This class holds a environment set of template_environment that has the same name
    //
    class template_set_environment RILL_CXX11_FINAL
        : public has_parameter_environment_base
    {
    public:
        template_set_environment( environment_parameter_t&& pp )
            : has_parameter_environment_base( std::move( pp ) )
            , inner_env_symbol_kind_( kind::type_value::e_none )
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

        //
        auto get_inner_env_symbol_kind() const
            -> kind::type_value
        {
            return inner_env_symbol_kind_;
        }

        auto set_inner_env_symbol_kind( kind::type_value const& k )
            -> void
        {
            inner_env_symbol_kind_ = k;
        }



        template<typename... Args>
        auto allocate_inner_env( Args&&... args )
            -> template_environment_ptr
        {
            // NOTE: unlike has_parameter_environment, the parant environment is "this env".
            return allocate_env<template_environment>( get_id(), std::forward<Args>( args )... );
        }

        auto get_candidates() const
            -> std::list<std::shared_ptr<template_environment>> const&
        {
            return template_candidates_list_;
        }

        auto add_candidate( std::shared_ptr<template_environment> const& template_env )
            -> void
        {
            // template_candidates_.emplace( template_env->get_parameter_num(), template_env );
            template_candidates_.insert( {
                    template_env->get_parameter_num(),
                    template_env
                } );
            template_candidates_list_.push_back( template_env );
        }


        // delegate lookup
        auto lookup( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer RILL_CXX11_OVERRIDE { assert(false); return nullptr; }
        auto lookup( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE { assert(false); return nullptr; }

        auto find_on_env( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer RILL_CXX11_OVERRIDE { assert(false); return nullptr; }
        auto find_on_env( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE { assert(false); return nullptr; }

    private:
        kind::type_value inner_env_symbol_kind_;

        std::multimap<template_environment::template_argument_length_type, std::shared_ptr<template_environment>> template_candidates_;
        std::list<std::shared_ptr<template_environment>> template_candidates_list_;
    };

} // namespace rill
