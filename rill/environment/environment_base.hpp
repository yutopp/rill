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
#include <boost/range/iterator_range.hpp>

#include <boost/algorithm/string/join.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"
#include "environment_kind.hpp"

#include "detail/mapper.hpp"

#include "environment_registry.hpp"
#include "global_environment_fwd.hpp"

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


    template<typename To, typename Env>
    inline auto cast_to( std::shared_ptr<Env> const& p )
        -> std::shared_ptr<To>
    {
        return std::static_pointer_cast<To>(
            p->checked_instance( To::KindValue )
            );
    }

    template<typename To, typename Env>
    inline auto cast_to( std::shared_ptr<Env const> const& p )
        -> std::shared_ptr<To const>
    {
        return std::static_pointer_cast<To const>(
            p->checked_instance( To::KindValue )
            );
    }



    // scoped_environment is base of module's environment(function, class, etc...)
    class environment_base
        : public std::enable_shared_from_this<environment_base>
    {
        friend global_environment;

    public:
        typedef environment_base                        env_type;

        typedef std::shared_ptr<env_type>               env_base_pointer;
        typedef std::shared_ptr<env_type const>         const_env_base_pointer;
        typedef std::weak_ptr<env_type>                 weak_env_base_pointer;
        typedef std::weak_ptr<env_type const>           const_weak_env_base_pointer;

        typedef ast::native_string_t                    native_string_type;

        weak_global_environment_ptr b_;

    public:
        // construct as a ROOT environment
        environment_base( root_initialize_tag, weak_global_environment_ptr const& b )
            : b_( b )
            , id_( environment_id_undefined )
            , progress_( environment_process_progress_t::constructed )
            , forward_referenceable_( true )
            , decl_order_( 0 )
            , do_mark_child_env_as_forward_referenceable_( true )
            , next_child_env_order_( nullptr )
        {
            std::cout << ">> environment constructed" << std::endl;
        }

        // normal constructor
        environment_base(
            environment_parameter_t const& ep
            )
            : b_( ep.global_env )
            , id_( ep.id )
            , parent_( ep.parent )
            , progress_( environment_process_progress_t::constructed )
            , forward_referenceable_( ep.forward_referenceable )
            , decl_order_( ep.decl_order )
            , do_mark_child_env_as_forward_referenceable_( ep.do_mark_child_env_as_forward_referenceable )
            , next_child_env_order_( ep.next_child_env_order )
        {
            std::cout << ">> environment constructed(as a child)"
                      << " / id: " << id_ << std::endl;
        }

        virtual ~environment_base()
        {

            std::cout << "<< environment destructed / id: " << id_ << " = " << typeid(*this).name() << std::endl;
        }

    public:
        //
        virtual auto lookup( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer;
        virtual auto lookup( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer;

        //
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer;
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer;


        // support functions, search identifier from native_string as NON templated
        auto lookup( ast::native_string_t const& name )
            -> env_base_pointer { return lookup( std::make_shared<ast::identifier_value>( name ) ); }
        auto lookup( ast::native_string_t const& name ) const
            -> const_env_base_pointer { return lookup( std::make_shared<ast::identifier_value>( name ) ); }

        //
        auto find_on_env( ast::native_string_t const& name )
            -> env_base_pointer { return find_on_env( std::make_shared<ast::identifier_value>( name ) ); }
        auto find_on_env( ast::native_string_t const& name ) const
            -> const_env_base_pointer { return find_on_env( std::make_shared<ast::identifier_value>( name ) ); }



        //
        auto lookup_layer( kind::type_value const& layer_type )
            -> env_base_pointer
        {
            auto p = shared_from_this();
            for(;;) {
                assert( p != nullptr );

                if ( p->get_symbol_kind() == layer_type )
                    return p;

                if ( p->has_parent() )
                    p = p->get_parent_env();
                else
                    break;
            }

            return nullptr;
        }

        auto lookup_layer( kind::type_value const& layer_type ) const
            -> const_env_base_pointer
        {
            auto p = shared_from_this();
            for(;;) {
                assert( p != nullptr );

                if ( p->get_symbol_kind() == layer_type )
                    return p;

                if ( p->has_parent() )
                    p = p->get_parent_env();
                else
                    break;
            }

            return nullptr;
        }

        // returns environment kind
        virtual auto get_symbol_kind() const
            -> kind::type_value =0;

        auto root_env()
            -> env_base_pointer
        {
            auto p = shared_from_this();
            while( !p->is_root() )
                p = p->get_parent_env();

            return p;
        }

        auto root_env() const
            -> const_env_base_pointer
        {
            auto p = shared_from_this();
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





        auto get_id() const
            -> environment_id_t
        {
            return id_;
        }


        virtual auto is_root() const -> bool { return false; }

        auto has_parent() const -> bool {
            return !is_root(); // if not a root, it has a parent environment
        }

        auto get_parent_env() -> env_base_pointer { return is_root() ? nullptr : parent_.lock(); }
        auto get_parent_env() const -> const_env_base_pointer { return is_root() ? nullptr : parent_.lock(); }



        //
        //
        //
        auto mark_as(
            kind::template_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            )
            -> std::pair<multiple_set_environment_ptr, template_environment_ptr>;

        auto mark_as(
            kind::function_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            )
            -> std::pair<multiple_set_environment_ptr, function_symbol_environment_ptr>;

        auto mark_as(
            kind::class_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            )
            -> std::pair<multiple_set_environment_ptr, class_symbol_environment_ptr>;

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
            kind::template_tag,
            ast::identifier_value_base_ptr const&
            )
            -> std::pair<multiple_set_environment_ptr, template_environment_ptr>;

        auto incomplete_construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const&
            )
            -> std::pair<multiple_set_environment_ptr, function_symbol_environment_ptr>;

        auto incomplete_construct(
            kind::class_tag,
            ast::identifier_value_base_ptr const&
            )
            -> std::pair<multiple_set_environment_ptr, class_symbol_environment_ptr>;

        auto incomplete_construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&
            )
            -> variable_symbol_environment_ptr;



        //
        // incomplete_construct
        //
        typedef std::function<function_symbol_environment_ptr (function_symbol_environment_ptr const&)> function_env_generator_scope_type;
        auto construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            function_env_generator_scope_type const&,
            class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> function_symbol_environment_ptr;

        auto construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            const_class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> variable_symbol_environment_ptr;

        auto construct(
            kind::class_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> class_symbol_environment_ptr;

    public:
        virtual auto checked_instance( kind::type_value const& e )
            -> env_base_pointer
        {
            if ( get_symbol_kind() != e ) return nullptr;

            return shared_from_this();
        }

        virtual auto checked_instance( kind::type_value const& e ) const
            -> const_env_base_pointer
        {
            if ( get_symbol_kind() != e ) return nullptr;

            return shared_from_this();
        }



    public:
        //
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
        inline auto is_exist( native_string_type const& name ) const
            -> bool
        {
            return inner_envs_.find( name ) != inner_envs_.cend();
        }

    private:
        environment_id_t id_;
        weak_env_base_pointer parent_;

    private:
        environment_process_progress_t progress_;
        std::unordered_map<native_string_type, env_base_pointer> inner_envs_;   // children environments

    private:
        bool forward_referenceable_;
        std::size_t decl_order_;

        bool do_mark_child_env_as_forward_referenceable_;
        std::shared_ptr<std::size_t> next_child_env_order_;
    };

} // namespace rill
