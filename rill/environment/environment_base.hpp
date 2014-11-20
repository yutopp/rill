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
#include <vector>
#include <utility>
#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"
#include "environment_kind.hpp"

#include "detail/mapper.hpp"

#include "environment_registry.hpp"
#include "global_environment_fwd.hpp"
#include "module_id.hpp"

#include "../type/type_registry.hpp"

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"


namespace rill
{
    //
    enum class environment_process_progress_t
    {
        constructed,    // only constructed(incomplete status)
        checked,        // id is collected
        completed       // constructed perfectly
    };

    struct debug_allocate_counter
    {
        unsigned int value = 0;
    };




    class environment_unit
        : public std::enable_shared_from_this<environment_unit>
    {
    public:
        environment_unit( environment_parameter_t&& ep )
            : b_( ep.global_env )
            , id_( ep.id )
            , parent_( ep.parent )
            , owner_module_id_( ep.mod_id )
            , is_private_( ep.is_private )
        {
            rill_dout << ">> environment constructed(as a child)"
                      << " / id: " << id_ << std::endl;
        }

        virtual ~environment_unit()
        {
            rill_dout << "<< environment destructed / id: " << id_ << " = " << typeid(*this).name() << std::endl;
        }

    public:
        auto get_id() const
            -> environment_id_t
        {
            return id_;
        }

        auto is_root() const
            -> bool
        {
            // has not parent
            return parent_.use_count() == 0;
        }

        auto has_parent() const
            -> bool
        {
            return !is_root();
        }

        auto get_parent_env()
            -> std::shared_ptr<environment_unit>
        {
            return is_root() ? nullptr : parent_.lock();
        }

        auto get_parent_env() const
            -> std::shared_ptr<environment_unit const>
        {
            return is_root() ? nullptr : parent_.lock();
        }

        auto get_owner_module_id() const
            -> module_id_t const&
        {
            return owner_module_id_;
        }

        auto is_private() const
            -> bool
        {
            return is_private_;
        }

        auto is_private( bool const b )
            -> void
        {
            is_private_ = b;
        }

    public:
        virtual auto has_elements() const
            -> bool
        {
            return false;
        }

        // returns environment kind
        virtual auto get_symbol_kind() const
            -> kind::type_value =0;

    public:
        auto connect_from_ast( ast::const_ast_base_ptr const& ast )
            -> void;

        auto connect_to_ast( ast::statement_ptr const& ast )
            -> void;

        template<typename AstPtr>
        auto link_with_ast( AstPtr const& ast )
            -> void
        {
            connect_from_ast( ast );
            connect_to_ast( ast );
        }

        auto get_related_ast()
            -> ast::statement_ptr;

        auto get_related_ast() const
            -> ast::const_statement_ptr;

    public:
        weak_global_environment_ptr b_;

    private:
        environment_id_t id_;
        std::weak_ptr<environment_unit> parent_;

    private:
        module_id_t owner_module_id_;
        bool is_private_;
    };



    template<typename Env>
    inline auto cast_to_base( std::shared_ptr<Env> const& p )
        -> environment_base_ptr
    {
        assert( p != nullptr );

        if ( p->get_symbol_kind() == kind::type_value::e_alias ) {
            auto const& tp = std::static_pointer_cast<environment_unit>( p );
            return cast_to_base(
                std::static_pointer_cast<alias_environment>( tp )->get_reference()
                );
        }

        assert( p->has_elements() == true );
        return std::static_pointer_cast<environment_base>( p );
    }

    template<typename Env>
    inline auto cast_to_base( std::shared_ptr<Env const> const& p )
        -> const_environment_base_ptr
    {
        assert( p != nullptr );

        if ( p->get_symbol_kind() == kind::type_value::e_alias ) {
            auto const& tp = std::static_pointer_cast<environment_unit const>( p );
            return cast_to_base(
                std::static_pointer_cast<alias_environment const>( tp )->get_reference()
                );
        }

        assert( p->has_elements() == true );
        return std::static_pointer_cast<environment_base const>( p );
    }



    // scoped_environment is base of module's environment(function, class, etc...)
    class environment_base
        : public environment_unit
    {
        friend global_environment;

    public:
        typedef environment_base                        env_type;

        typedef std::shared_ptr<env_type>               env_base_pointer;
        typedef std::shared_ptr<env_type const>         const_env_base_pointer;
        typedef std::weak_ptr<env_type>                 weak_env_base_pointer;
        typedef std::weak_ptr<env_type const>           const_weak_env_base_pointer;

        typedef ast::native_string_t                    native_string_type;

    public:
        environment_base(
            environment_parameter_t&& ep
            )
            : environment_unit( std::move( ep ) )
            , progress_( environment_process_progress_t::constructed )
            , closed_( false )
            , parent_class_env_id_( environment_id_undefined )

        {}

        virtual ~environment_base()
        {}

    public:
        virtual auto has_elements() const
            -> bool override final
        {
            return true;
        }

    public:
        // hiding
        auto get_parent_env()
            -> env_base_pointer;
        auto get_parent_env() const
            -> const_env_base_pointer;


        //
        virtual auto lookup(
            ast::const_identifier_value_base_ptr const& name,
            kind::type_value const& exclude_env_type = kind::type_value::e_none
            )
            -> env_base_pointer;
        virtual auto lookup(
            ast::const_identifier_value_base_ptr const& name,
            kind::type_value const& exclude_env_type = kind::type_value::e_none
            ) const
            -> const_env_base_pointer;

        //
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer;
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer;


        // support functions, search identifier from native_string as NON templated
        inline auto lookup(
            ast::native_string_t const& name,
            kind::type_value const& exclude_env_type = kind::type_value::e_none
            )
            -> env_base_pointer
        {
            return lookup(
                std::make_shared<ast::identifier_value const>( name ),
                exclude_env_type
                );
        }
        inline auto lookup(
            ast::native_string_t const& name,
            kind::type_value const& exclude_env_type = kind::type_value::e_none
            ) const
            -> const_env_base_pointer
        {
            return lookup(
                std::make_shared<ast::identifier_value const>( name ),
                exclude_env_type
                );
        }

        //
        inline auto find_on_env( ast::native_string_t const& name )
            -> env_base_pointer
        {
            return find_on_env( std::make_shared<ast::identifier_value const>( name ) );
        }
        inline auto find_on_env( ast::native_string_t const& name ) const
            -> const_env_base_pointer
        {
            return find_on_env( std::make_shared<ast::identifier_value const>( name ) );
        }


        //
        auto lookup_layer( kind::type_value const& layer_type )
            -> env_base_pointer;
        auto lookup_layer( kind::type_value const& layer_type ) const
            -> const_env_base_pointer;

        auto root_env()
            -> env_base_pointer
        {
            auto p = std::static_pointer_cast<env_type>( shared_from_this() );
            while( !p->is_root() ) {
                p = p->get_parent_env();
            }

            return p;
        }

        auto root_env() const
            -> const_env_base_pointer
        {
            auto p = std::static_pointer_cast<env_type const>( shared_from_this() );
            while( !p->is_root() )
                p = p->get_parent_env();

            return p;
        }

        auto lookup_on_root( ast::const_identifier_value_ptr const& type_name )
            -> env_base_pointer
        {
            return root_env()->lookup( type_name );
        }
        auto lookup_on_root( ast::const_identifier_value_ptr const& type_name ) const
            -> const_env_base_pointer
        {
            return root_env()->lookup( type_name );
        }


        //
        //
        auto import_from( const_environment_base_ptr const& )
            -> void;

        //
        //
        void set_parent_class_env_id( environment_id_t const& parent_class_env_id )
        {
            parent_class_env_id_ = parent_class_env_id;
        }

        auto get_parent_class_env_id() const
            -> environment_id_t
        {
            return parent_class_env_id_;
        }

        bool is_in_class() const
        {
            return parent_class_env_id_ != environment_id_undefined;
        }


        //
        //
        //
        auto mark_as(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            )
            -> variable_symbol_environment_ptr;


        //
        // incomplete_construct
        //
        // "incomplete_construct" contructs an environment that is incomplete.
        // incomplete means this environment is in making phase
        //
        auto incomplete_construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&
            )
            -> variable_symbol_environment_ptr;


        auto incomplete_construct(
            kind::multiset_tag,
            ast::identifier_value_base_ptr const&
            )
            -> multiple_set_environment_ptr;

        //
        // incomplete_construct
        //

        auto construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            const_class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> variable_symbol_environment_ptr;

    public:
        virtual auto checked_instance( kind::type_value const& e )
            -> env_base_pointer
        {
            if ( get_symbol_kind() != e ) return nullptr;

            return std::static_pointer_cast<environment_base>( shared_from_this() );
        }

        virtual auto checked_instance( kind::type_value const& e ) const
            -> const_env_base_pointer
        {
            if ( get_symbol_kind() != e ) return nullptr;

            return std::static_pointer_cast<environment_base const>( shared_from_this() );
        }









        ///
        /// virtual auto mangled_name() const -> ast::native_string_t { return ""; }

        ///
        ///
        ///
        virtual auto dump( std::ostream& os, std::string const& indent ) const
            -> std::ostream&
        { return os; }

        auto dump_include_env( std::ostream& os, std::string const& indent ) const
            -> std::ostream&
        {
            os << indent << "==" << std::endl;
            for( auto&& ins : inner_envs_ ) {
                auto const& key = ins.first;
                auto const& env = ins.second;

                auto const& ast = env->get_related_ast();

                os << indent
                   << "-> symbol_name: " << key
                   << " / id: " << env->get_id()
                   << " / linked_astptr: " << ast.get()
                   << " / symbol kind: " << debug_string( env->get_symbol_kind() ) << std::endl;
            }

            return os;
        }






    public:
        //
        auto is_incomplete() const
            -> bool
        {
            return progress_ == environment_process_progress_t::constructed;
        }

        auto is_checked() const
            -> bool
        {
            return progress_ >= environment_process_progress_t::checked;
        }

        auto is_complete() const
            -> bool
        {
            return progress_ >= environment_process_progress_t::completed;
        }


        //
        auto change_progress_to_checked()
            -> void
        {
            progress_ = environment_process_progress_t::checked;
        }

        auto change_progress_to_completed()
            -> void
        {
            progress_ = environment_process_progress_t::completed;
        }

    public:
        inline auto is_exist( native_string_type const& name )
            -> boost::optional<environment_unit_ptr>
        {
            auto it = inner_envs_.find( name );
            if ( it == inner_envs_.cend() ) {
                return boost::none;
            }

            return boost::make_optional(
                it->second
                );
        }

        inline auto is_exist( native_string_type const& name ) const
            -> boost::optional<const_environment_unit_ptr>
        {
            auto it = inner_envs_.find( name );
            if ( it == inner_envs_.cend() ) {
                return boost::none;
            }

            return boost::make_optional(
                std::static_pointer_cast<environment_unit const>(
                    it->second
                    )
                );
        }

        inline auto is_exist( ast::identifier_value_base_ptr const& name )
        {
            return is_exist( name->get_inner_symbol()->to_native_string() );
        }

        inline auto is_exist( ast::const_identifier_value_base_ptr const& name ) const
        {
            return is_exist( name->get_inner_symbol()->to_native_string() );
        }

    public:
        // closed means environment has at least one flow always jumps to elsewhare.
        inline auto is_closed() const
            -> bool
        {
            return closed_;
        }

        inline auto mask_is_closed( bool const b )
            -> void
        {
            closed_ |= b;
        }

    private:
        environment_process_progress_t progress_;
        std::unordered_map<native_string_type, environment_unit_ptr> inner_envs_;   // children environments
        bool closed_;

        environment_id_t parent_class_env_id_;
    };




    template<typename To, typename Env>
    inline auto cast_to( std::shared_ptr<Env> const& p )
        -> std::shared_ptr<To>
    {
        return std::static_pointer_cast<To>(
            cast_to_base( p )->checked_instance( To::KindValue )
            );
    }

    template<typename To, typename Env>
    inline auto cast_to( std::shared_ptr<Env const> const& p )
        -> std::shared_ptr<To const>
    {
        return std::static_pointer_cast<To const>(
            cast_to_base( p )->checked_instance( To::KindValue )
            );
    }

} // namespace rill
