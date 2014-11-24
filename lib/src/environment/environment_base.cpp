//
// Copyright yutopp 2014 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment/global_environment.hpp>
#include <rill/environment/root_environment.hpp>
#include <rill/environment/environment.hpp>


#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    auto environment_base::get_parent_env()
        -> env_base_pointer
    {
        return cast_to_base( environment_unit::get_parent_env() );
    }

    auto environment_base::get_parent_env() const
        -> const_env_base_pointer
    {
        return cast_to_base( environment_unit::get_parent_env() );
    }


    auto environment_base::lookup(
        ast::const_identifier_value_base_ptr const& identifier,
        kind::type_value const& exclude_env_type
        )
        -> env_base_pointer
    {
        rill_dout << debug_string( get_symbol_kind() ) << std::endl;
        if ( get_symbol_kind() == exclude_env_type ) {
            return is_root()
                ? nullptr
                : get_parent_env()->lookup( identifier, exclude_env_type );
        }

        // find symbol in self environment
        auto const s = find_on_env( identifier );
        if ( s == nullptr ) {
            return is_root()
                ? nullptr
                : get_parent_env()->lookup( identifier, exclude_env_type );
        }

        return s;
    }

    auto environment_base::lookup(
        ast::const_identifier_value_base_ptr const& identifier,
        kind::type_value const& exclude_env_type
        ) const
        -> const_env_base_pointer
    {
        rill_dout << debug_string( get_symbol_kind() ) << std::endl;
        if ( get_symbol_kind() == exclude_env_type ) {
            return is_root()
                ? nullptr
                : get_parent_env()->lookup( identifier, exclude_env_type );
        }

        auto const s = find_on_env( identifier );
        if ( s == nullptr ) {
            return is_root()
                ? nullptr
                : get_parent_env()->lookup( identifier, exclude_env_type );
        }

        return s;
    }

    auto environment_base::find_on_env( ast::const_identifier_value_base_ptr const& identifier )
        -> env_base_pointer
    {
        auto const& name = identifier->get_inner_symbol()->to_native_string();

        // try to find in inner_envs_
        auto&& it = inner_envs_.find( name );
        if ( it != inner_envs_.end() ) {
            return cast_to_base( it->second );
        }

        // failed...
        return nullptr;
    }

    auto environment_base::find_on_env( ast::const_identifier_value_base_ptr const& identifier ) const
        -> const_env_base_pointer
    {
        auto const& name = identifier->get_inner_symbol()->to_native_string();

        auto&& it = inner_envs_.find( name );
        if ( it != inner_envs_.end() ) {
            return cast_to_base( it->second );
        }

        return nullptr;
    }


    auto environment_base::import_from(
        const_environment_base_ptr const& from
        )
        -> void
    {
        bool import_as_public = false;

        // make alias to top level envs
        for( auto&& env_unit : from->inner_envs_ ) {
            auto const& name = std::get<0>( env_unit );
            auto const& target_env = std::get<1>( env_unit );

            rill_dout << "import: " << name << std::endl;
            if ( target_env->is_private() ) {
                rill_dout << "  - private" << std::endl;
                continue;
            }

            if ( from->get_owner_module_id() != target_env->get_owner_module_id() ) {
                rill_dout << "  - other packages" << std::endl;
                continue;
            }

            auto alias_env
                = b_.lock()->template allocate_env<alias_environment>(
                    shared_from_this(),
                    target_env
                    );
            if ( !import_as_public ) {
                rill_dout << "  ! NOT public import" << std::endl;
                alias_env->is_private( true );
            }

            // copy!
            inner_envs_[name] = alias_env;
        }
    }


    auto environment_base::lookup_layer( kind::type_value const& layer_type )
        -> env_base_pointer
    {
        auto p = std::static_pointer_cast<env_type>( shared_from_this() );
        for(;;) {
            assert( p != nullptr );

            if ( p->get_symbol_kind() == layer_type ) {
                return p;
            }

            if ( p->has_parent() ) {
                p = p->get_parent_env();

            } else {
                break;
            }
        }

        return nullptr;
    }

    auto environment_base::lookup_layer( kind::type_value const& layer_type ) const
        -> const_env_base_pointer
    {
        auto p = std::static_pointer_cast<env_type const>( shared_from_this() );
        for(;;) {
            assert( p != nullptr );

            if ( p->get_symbol_kind() == layer_type ) {
                return p;
            }

            if ( p->has_parent() ) {
                p = p->get_parent_env();

            } else {
                break;
            }
        }

        return nullptr;
    }


    //
    auto environment_base::incomplete_construct(
        kind::multiset_tag,
        ast::identifier_value_base_ptr const& name
        )
        -> multiple_set_environment_ptr
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // wrapper environment
        return b_.lock()->allocate_env_unless_exist<multiple_set_environment>(
            std::static_pointer_cast<environment_base>( shared_from_this() ),
            symbol_name,
            symbol_name
            );
    }

    auto environment_base::incomplete_construct(
        kind::variable_tag,
        ast::const_identifier_value_base_ptr const& name
        )
        -> variable_symbol_environment_ptr
    {
        assert( name != nullptr );

        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        return b_.lock()->allocate_env_unless_exist<variable_symbol_environment>(
            std::static_pointer_cast<environment_base>( shared_from_this() ),
            symbol_name,
            symbol_name
            );
    }


    auto environment_base::append_outer_referenced(
        outer_referenced_ast_ptr_type const& node
        )
        -> void
    {
        return outer_referenced_asts_.emplace_back( node );
    }

    auto environment_base::get_outer_referenced_asts() const
        -> outer_referenced_asts_type
    {
        outer_referenced_asts_type xs;
        get_outer_referenced_asts( xs );

        return xs;
    }

    auto environment_base::get_outer_referenced_asts(
        outer_referenced_asts_type& v
        ) const
        -> void
    {
        rill_dout << "!!!!! " << outer_referenced_asts_.size() << " / " << this << std::endl;
        std::copy(
            outer_referenced_asts_.cbegin(),
            outer_referenced_asts_.cend(),
            std::back_inserter( v )
            );
        for( auto&& inner_env : inner_envs_ ) {
            cast_to_base( inner_env.second )->get_outer_referenced_asts( v );
        }
    }

} // namespace rill
