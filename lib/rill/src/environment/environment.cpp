//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // debug

#include <rill/environment/environment.hpp>

#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    std::ostream& operator<<( std::ostream& os, const_environment_base_ptr const& env )
    {
        os << "DEBUG: environment" << std::endl;
        auto e = env;
        std::string indent = "^ ";

        if ( e ) {
            while( !e->is_root() ) {
                e->dump( os, indent );
                e = e->get_parent_env();
                indent += "  ";
            }
            return e->dump( os, indent );
        } else {
            os << indent << "nullptr." << std::endl;
            return os;
        }
    }




//
    auto single_identifier_environment_base::find_on_env( ast::const_identifier_value_base_ptr const& name )
        -> env_base_pointer
    {
        auto const it = instanced_env_.find( name->get_inner_symbol()->to_native_string() );

        return ( it != instanced_env_.end() ) ? it->second : nullptr;
    }

    auto single_identifier_environment_base::find_on_env( ast::const_identifier_value_base_ptr const& name ) const
        -> const_env_base_pointer
    {
        auto const it = instanced_env_.find( name->get_inner_symbol()->to_native_string() );

        return ( it != instanced_env_.end() ) ? it->second : nullptr;
    }


    auto single_identifier_environment_base::lookup( ast::const_identifier_value_base_ptr const& name )
        -> env_base_pointer
    {
        if ( name->is_template() ) {
            // TODO: add template support
            assert( false );
            return nullptr;

        } else {
            auto const s = find_on_env( name );

            return s
                ? s
                : is_root()
                    ? nullptr   // Not found...
                    : get_parent_env()->lookup( name )
                ;
        }
    }

    auto single_identifier_environment_base::lookup( ast::const_identifier_value_base_ptr const& name ) const
        -> const_env_base_pointer
    {
        if ( name->is_template() ) {
            // TODO: add template support
            return nullptr;

        } else {
            auto const s = find_on_env( name );

            return s
                ? s
                : is_root()
                ? nullptr
                : get_parent_env()->lookup( name )
                ;
        }
    }





    auto function_symbol_environment::mangled_name() const
        -> native_string_type
    {
        // TODO: call parent mangled_name()



        return name_ + make_parameter_hash( this, parameter_type_ids_ );
//        return name_ + make_parameter_hash( parameter_type_ids_ );
    }


    auto class_symbol_environment::mangled_name() const
        -> native_string_type
    {
        return name_;
    }


} // namespace rill
