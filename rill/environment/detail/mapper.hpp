//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef RILL_ENVIRONMENT_MAPPER
#define RILL_ENVIRONMENT_MAPPER

#include <unordered_map>
#include <memory>

#include "../environment_fwd.hpp"

#include "../../ast/statement.hpp"
#include "../../type/type_registry_fwd.hpp"


namespace rill
{
    // table[ast_ptr -> [env_id] env_ptr]
    class ast_to_environment_id_mapper
    {
    public:
        typedef ast::ast_id_t                  key_type;
        //typedef environment_id_t                value_type;
        typedef const_environment_unit_ptr     value_type;

    public:
        template<typename SmartPtr>
        auto add( SmartPtr const& ast_ptr, value_type const& env_ptr )
            -> void
        {
            assert( ast_ptr != nullptr );

            if ( map_.find( ast_ptr->get_id() ) != map_.cend() ) {
                rill_dout << "Already mapped... :: env id -> " << map_.at( ast_ptr->get_id() )->get_id() << std::endl;
                return;
            }

            map_.emplace( ast_ptr->get_id(), env_ptr );
            //map_[ast_ptr.get()] = env_ptr;
            assert( map_.at( ast_ptr->get_id() ) == env_ptr && "tried to reassign" );
        }

        template<typename SmartPtr>
        auto get( SmartPtr const& ast_ptr ) const
            -> environment_id_t
        {
            assert( ast_ptr != nullptr );

            rill_dout << "ptr-> " << ast_ptr << std::endl;
            return ( map_.find( ast_ptr->get_id() ) != map_.cend() )
                ? ( map_.at( ast_ptr->get_id() )->get_id() )
                : environment_id_undefined;
        }

    private:
        std::unordered_map<key_type, value_type> map_;
    };



    // table[env_id -> ast_ptr]
    class environment_id_to_ast_mapper
    {
    public:
        typedef environment_id_t            key_type;
        typedef ast::statement_ptr          value_type;
        typedef ast::const_statement_ptr    const_value_type;

    public:
        template<typename Id, typename AstPtr>
        auto add( Id const& env_id, AstPtr const& ast_ptr )
            -> void
        {
            // TODO: add dup check
            assert( ast_ptr != nullptr );

            map_.emplace( env_id, ast_ptr );
            //map_[env_id] = ast_ptr;
            assert( map_.at( env_id ) == ast_ptr );
        }

        template<typename Id>
        auto get( Id const& env_id ) const
            -> value_type
        {
            rill_dout << "env id-> " << env_id << std::endl;
            return ( map_.find( env_id ) != map_.cend() ) ? map_.at( env_id ) : nullptr;
        }

    private:
        std::unordered_map<key_type, value_type> map_;
    };


    // table[ast_ptr -> type_id]
    class ast_to_type_id_mapper
    {
    public:
        typedef ast::const_ast_base_ptr     key_type;
        typedef type_id_t                   value_type;

    public:
        template<typename SmartPtr>
        auto add( SmartPtr const& ast_ptr, value_type const& tid )
            -> void
        {
            map_.emplace( ast_ptr, tid );
            assert( map_.at( ast_ptr ) == tid && "maybe, duplicate add" );
        }

        template<typename SmartPtr>
        auto get( SmartPtr const& ast_ptr ) const
            -> type_id_t
        {
            assert( ast_ptr != nullptr );

            rill_dout << "ast ptr-> " << ast_ptr << std::endl;
            auto it = map_.find( ast_ptr );
            return ( it != map_.cend() ) ? ( it->second ) : type_id_undefined;
        }

    private:
        std::unordered_map<key_type, value_type> map_;
    };
} // namespace rill

#endif /*RILL_ENVIRONMENT_MAPPER*/
