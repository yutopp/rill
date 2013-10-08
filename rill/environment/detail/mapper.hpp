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

#include "../../environment_fwd.hpp"

#include "../../ast/statement.hpp"
#include "../../ast/detail/dispatch_assets.hpp"


namespace rill
{
    // table[ast_ptr -> env_id]
    class ast_to_environment_id_mapper
    {
    public:
        typedef void const*                     key_type;
        typedef environment_id_t                value_type;

    public:
        template<typename SmartPtr, typename Id>
        auto add( SmartPtr const& ast_ptr, Id const& env_id )
            -> void
        {
            map_.emplace( ast_ptr.get(), env_id );
        }

        template<typename Id>
        auto add( key_type const& address, Id const& env_id )
            -> void
        {
            map_.emplace( address, env_id );
        }

        template<typename SmartPtr>
        auto get( SmartPtr const& ast_ptr ) const
            -> value_type
        {
            std::cout << "ptr-> " << ast_ptr.get() << std::endl;
            return ( map_.find( ast_ptr.get() ) != map_.cend() ) ? map_.at( ast_ptr.get() ) : environment_id_undefined;
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

            map_.emplace( env_id, ast_ptr );
        }

        template<typename Id>
        auto get( Id const& env_id ) const
            -> value_type
        {
            return ( map_.find( env_id ) != map_.cend() ) ? map_.at( env_id ) : nullptr;
        }

    private:
        std::unordered_map<key_type, value_type> map_;
    };

#if 0
    // table[env_id -> {env_id, ast_ptr}]
    class environment_to_asts_mapper
    {
    public:
        typedef environment_id_t        key_type;
        struct value_type
        {
            environment_id_t related_env_id;
            ast::statement_ptr statement_ast;
        };
        typedef std::unordered_multimap<key_type, value_type> container_type;
        typedef container_type::const_iterator const_iterator_type;
        typedef std::pair<container_type::const_iterator, container_type::const_iterator> const_iterator_pair_type;
    public:
        template<typename Id, typename AstPtr>
        auto add( Id const& env_id, Id const& env_id_on_time, AstPtr const& ast_ptr )
            -> void
        {
            value_type vv = { env_id_on_time, ast_ptr };
            map_.emplace( env_id, vv );
        }

        template<typename Id>
        auto get( Id const& env_id ) const
            -> const_iterator_pair_type
        {
            return map_.equal_range( env_id );
        }

    private:
        container_type map_;
    };
#endif
} // namespace rill

#endif /*RILL_ENVIRONMENT_MAPPER*/
