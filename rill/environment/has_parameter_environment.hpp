//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <string>
#include <functional>
#include <vector>
#include <list>

#include <boost/algorithm/string/join.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include "has_parameter_environment_base.hpp"


namespace rill
{
    //
    // 
    //
    typedef std::string parameter_hash_t;

    template<typename EnvRawPtr, typename TypeIds>
    inline auto make_parameter_hash( EnvRawPtr const& env, TypeIds const& type_ids_list )
        -> parameter_hash_t
    {
        {
            // debug:
            for( auto const& i : type_ids_list )
                std::cout << ":" << i << " ";
            std::cout << std::endl;
        }
/*
        auto const& aaa = 
        variable_env_id_list
            | boost::adaptors::transformed(
                std::function<const_variable_symbol_environment_ptr (environment_id_t const&)>( [&]( environment_id_t const& id )
                                                           {
                                                               return std::static_pointer_cast<variable_symbol_environment const>( env->get_env_strong_at( id ) );
                                                           } )
                )
            ;
*/

        return std::to_string( type_ids_list.size() )
                + "_"
                + boost::algorithm::join(
                    type_ids_list
                    | boost::adaptors::transformed(
                        std::function<std::string (type_id_t const&)>( []( type_id_t const& id ){ return std::to_string( id ); } )
                        ),
                    "%"
                    );
    }



    template<typename InlineEnvironment>
    class has_parameter_environment RILL_CXX11_FINAL
        : public has_parameter_environment_base
    {
    public:
        static kind::type_value const KindValue = InlineEnvironment::KindValue;

    public:
        has_parameter_environment( environment_id_t const id, weak_env_base_pointer const& parent )
            : has_parameter_environment_base( id, parent )
        {}

    public:
        auto get_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return kind::type_value::e_parameter_wrapper;
        }

        auto get_inner_symbol_kind() const
            -> kind::type_value RILL_CXX11_OVERRIDE
        {
            return KindValue;
        }

        template<typename... Args>
        auto allocate_inner_env( Args&&... args )
            -> std::shared_ptr<InlineEnvironment>
        {
            // NOTE: parant environment is not this env but one rank top env
            return allocate_env<InlineEnvironment>( get_parent_env(), get_id(), std::forward<Args>( args )... );
        }

        template<typename... Args>
        auto allocate_inner_env_as_incomplete( Args&&... args )
            -> std::shared_ptr<InlineEnvironment>
        {
            auto const inner_env = allocate_inner_env( std::forward<Args>( args )... );
            incomplete_inners_.push_back( inner_env );

            return inner_env;
        }

        auto get_incomplete_inners() const
            -> std::vector<std::shared_ptr<InlineEnvironment>> const&
        {
            return incomplete_inners_;
        }

        auto add_overload( std::shared_ptr<InlineEnvironment> const& inner_env )
            -> env_base_pointer
        {
            // TODO: add duplicate check
            overloads_[make_parameter_hash( this, inner_env->get_parameter_type_ids() )] = inner_env;

            overloads_2_.push_back( inner_env );

            return inner_env;
        }

        auto solve_overload( type_id_list_t const& arg_type_ids ) const
            -> std::shared_ptr<InlineEnvironment>
        {
            std::cout << "solve_overload? hash: " << make_parameter_hash( this, arg_type_ids ) << std::endl;

            auto const it = overloads_.find( make_parameter_hash( this, arg_type_ids ) );

            return it != overloads_.cend() ? it->second : nullptr;
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
    /*
        auto lookup( environment_ptr const& parent, parameter_list const& parameter ) const
            -> const_environment_ptr
        {
            if ( parameter.size() == 2 ) {
                return p_;
                //if ( parameter[0].type->type()-> )
            }

            return nullptr;
        }*/



        auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream& RILL_CXX11_OVERRIDE
        {
            os  << indent << "has_parameter_environment" << std::endl;
            for( auto const& p : overloads_ ) {
                os << indent << p.first << std::endl
                   << indent << (environment_base_ptr const&)p.second << std::endl
                   << indent << "======" << std::endl;
            }
            return os;
        }

        auto get_overloads()
            -> std::list<std::shared_ptr<InlineEnvironment>>&
        {
            return overloads_2_;
        }

    private:
        std::vector<std::shared_ptr<InlineEnvironment>> incomplete_inners_;
        std::unordered_map<parameter_hash_t, std::shared_ptr<InlineEnvironment>> overloads_;


        std::list<std::shared_ptr<InlineEnvironment>> overloads_2_;
    };

} // namespace rill
