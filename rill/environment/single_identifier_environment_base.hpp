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

#include "../config/macros.hpp"

#include "environment_base.hpp"


namespace rill
{
    //
    // 
    //
    class single_identifier_environment_base
        : public environment_base
    {
        typedef single_identifier_environment_base      self_type;
        typedef std::shared_ptr<self_type>              self_pointer;
        typedef std::shared_ptr<self_type>              self_const_pointer;

    public:
        single_identifier_environment_base( root_initialize_tag )
            : environment_base( root_initialize_tag() )
        {}
    
        single_identifier_environment_base( environment_id_t const& id, weak_env_base_pointer const& parent )
            : environment_base( id, parent )
        {}

        virtual ~single_identifier_environment_base() {};

    public:
        virtual auto lookup( ast::intrinsic::const_single_identifier_value_base_ptr const& )
            -> env_base_pointer RILL_CXX11_OVERRIDE;
        virtual auto lookup( ast::intrinsic::const_single_identifier_value_base_ptr const& ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE;

        //
        virtual auto find_on_env( ast::intrinsic::const_single_identifier_value_base_ptr const& )
            -> env_base_pointer RILL_CXX11_OVERRIDE;
        virtual auto find_on_env( ast::intrinsic::const_single_identifier_value_base_ptr const& ) const
            -> const_env_base_pointer RILL_CXX11_OVERRIDE;





        //
        virtual auto is_incomplete() const
            -> bool { return false; }


    /*
        auto is_exist_in_instanced( native_string_type const& name ) const
            -> boost::optional<env_base_pointer>
        {
            auto const& it = instanced_env_.find( name );
            if ( it != instanced_env_.cend() )
                return it->second;

            return boost::none;
        }*/

        /*
        auto is_same_pre_declared_type( intrinsic::identifier_value_ptr const& name, symbol_kind const& kind ) const
            -> bool
        {

        }*/

        //
        // incomplete_construct
        //
        virtual auto incomplete_construct(
            kind::function_tag,
            ast::intrinsic::single_identifier_value_base_ptr const&
            ) -> std::pair<
                     std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                     function_symbol_environment_ptr
                 >;

        virtual auto incomplete_construct(
            kind::variable_tag,
            ast::intrinsic::single_identifier_value_base_ptr const&
            ) -> variable_symbol_environment_ptr;


        virtual auto incomplete_construct(
            kind::class_tag,
            ast::intrinsic::single_identifier_value_base_ptr const&
            ) -> class_symbol_environment_ptr;



        //
        // construct
        //
        typedef std::function<function_symbol_environment_ptr (function_symbol_environment_ptr const&)> function_env_generator_scope_type;
        virtual auto construct(
            kind::function_tag,
            ast::intrinsic::single_identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            function_env_generator_scope_type const&,
            class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> function_symbol_environment_ptr;


        virtual auto construct(
            kind::variable_tag,
            ast::intrinsic::single_identifier_value_base_ptr const&,
            const_class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> variable_symbol_environment_ptr;


        virtual auto construct(
            kind::class_tag,
            ast::intrinsic::single_identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> class_symbol_environment_ptr;




        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "single_identifier_environment_base" << std::endl;
            return dump_include_env( os, indent );
        }

        auto dump_include_env( std::ostream& os, std::string const& indent ) const
            -> std::ostream&
        {
            for( auto const& ins : instanced_env_ ) {
                os << indent
                   << "-> symbol: " << ins.first
                   << " / id: " << ins.second->get_id()
                   << " / symbol kind: " << static_cast<int>( ins.second->get_symbol_kind() ) << std::endl;
            }

            return os;
        }

        auto is_instanced( native_string_type const& name ) const
            -> bool
        {
            return instanced_env_.find( name ) != instanced_env_.end();
        }

    private:
        std::unordered_map<native_string_type, env_base_pointer> instanced_env_;
        std::unordered_map<native_string_type, std::shared_ptr<template_environment>> template_env_;
    };

} // namespace rill
