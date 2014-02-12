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

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "../config/macros.hpp"

#include "environment_fwd.hpp"
#include "environment_kind.hpp"

#include "detail/mapper.hpp"

#include "environment_registry.hpp"
#include "type_registry.hpp"

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"


namespace rill
{
    typedef unsigned int    symbol_types_mask_t;
    enum struct symbol_types : symbol_types_mask_t
    {
        root_c = ( 1u << 0 )
    };










    enum struct error_code
    {
    };





    enum struct typed_process
    {
        untyped,
        processing,
        typed
    };


    struct debug_allocate_counter
    {
        debug_allocate_counter() : value( 0 ) {}

        unsigned int value;
    };

    template<typename BaseEnvT>
    struct environment_shared_resource
    {
        typedef environment_registry<BaseEnvT>      environment_registry_type;
        typedef ast_to_environment_id_mapper        ast_to_env_id_mapper_type;
        typedef environment_id_to_ast_mapper        env_id_to_ast_mapper_type;
        typedef type_registry                       type_registry_type;

        environment_registry_type container;
        ast_to_env_id_mapper_type ast_to_env_id_map;
        env_id_to_ast_mapper_type env_id_to_ast_map;

        type_registry_type types_container;

        debug_allocate_counter debug_allocate_counter_;
    };




    struct root_initialize_tag {};



    static auto const unnamed = nullptr;




    class environment_base
        : public std::enable_shared_from_this<environment_base>
    {
    public:
        typedef environment_base                        env_type;

        typedef std::shared_ptr<env_type>               env_base_pointer;
        typedef std::shared_ptr<env_type const>         const_env_base_pointer;
        typedef std::weak_ptr<env_type>                 weak_env_base_pointer;
        typedef std::weak_ptr<env_type const>           const_weak_env_base_pointer;

        typedef ast::native_string_t                    native_string_type;
        typedef environment_shared_resource<env_type>   shared_resource_type;

    public:
        // construct as ROOT environment
        environment_base( root_initialize_tag )
            : id_( environment_id_undefined )
            , forward_referenceable_( true )
            , decl_order_( 0 )
            , root_shared_resource_( std::make_shared<environment_shared_resource<env_type>>() )
            , do_mark_child_env_as_forward_referenceable_( true )
            , next_child_env_order_( nullptr )
        {
            std::cout << ">> environment allocated" << " ( "  << root_shared_resource_->debug_allocate_counter_.value <<" )" << std::endl;

            ++root_shared_resource_->debug_allocate_counter_.value;
        }

        // normal constructor
        environment_base(
            environment_parameter_t const& ep
            )
            : id_( ep.id )
            , parent_( ep.parent )
            , forward_referenceable_( ep.forward_referenceable )
            , decl_order_( ep.decl_order )
            , root_shared_resource_( ep.parent.lock()->root_shared_resource_ )
            , do_mark_child_env_as_forward_referenceable_( ep.do_mark_child_env_as_forward_referenceable )
            , next_child_env_order_( ep.next_child_env_order )
        {
            std::cout << ">> environment allocated(inner): " << id_ << " ( "  << root_shared_resource_->debug_allocate_counter_.value <<" )"  << std::endl;

            ++root_shared_resource_->debug_allocate_counter_.value;
        }

        virtual ~environment_base()
        {
            --root_shared_resource_->debug_allocate_counter_.value;

            std::cout << "<< environment DEallocated: " << id_ << " ( "  << root_shared_resource_->debug_allocate_counter_.value <<" ) " << typeid( *this).name() << std::endl;
        }

    public:
        //
        virtual auto lookup( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer =0;
        virtual auto lookup( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer =0;

        //
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& name )
            -> env_base_pointer =0;
        virtual auto find_on_env( ast::const_identifier_value_base_ptr const& name ) const
            -> const_env_base_pointer =0;


        // this function will be deleted in the future...
        template<typename F>
        auto nest_lookup( ast::const_nested_identifier_value_ptr const& ids, F const& failed_callback )
            -> env_base_pointer
        {
            // current environment
            env_base_pointer env = shared_from_this();

            for( auto const& id : ids->get_nest_ids() ) {
                auto const temp_env = env;
                if ( env.get() == this ) {
                    env = env->lookup( id );
                } else {
                    env = env->find_on_env( id );
                }

                if ( env == nullptr ) {
                    // if failed to lookup, call recovery function
                    env = failed_callback( temp_env, id );
                    if ( env == nullptr )
                        break;
                }
            }
            return env;
        }

        auto nest_lookup( ast::const_nested_identifier_value_ptr const& ids )
            -> env_base_pointer
        {
            return nest_lookup( ids, []( env_base_pointer const&, ast::const_identifier_value_base_ptr const& ){ return nullptr; } );
        }

        template<typename F>
        auto nest_lookup( ast::const_nested_identifier_value_ptr const& ids ) const
            -> const_env_base_pointer
        {
            const_env_base_pointer env = shared_from_this();

            for( auto const& id : ids->get_nest_ids() ) {
                if ( env == shared_from_this() ) {
                    env = env->lookup( id );
                } else {
                    env = env->find_on_env( id );
                }

                if ( env == nullptr )
                    break;
            }
            return env;
        }





        //
        auto lookup_layer( kind::type_value const& layer_type )
            -> env_base_pointer
        {
            auto p = shared_from_this();
            for(;;) {
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
                if ( p->get_symbol_kind() == layer_type )
                    return p;

                if ( p->has_parent() )
                    p = p->get_parent_env();
                else
                    break;
            }

            return nullptr;
        }

        //
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




        template<typename Env, typename... Args>
        auto allocate_env( Args&&... args )
            -> typename shared_resource_type::environment_registry_type::template result<Env>::type
        {
            weak_env_base_pointer const& base_env
                = shared_from_this();
            bool const forward_referenceable
                = do_mark_child_env_as_forward_referenceable_;
            std::size_t const decl_order
                = forward_referenceable ? 0 : (*next_child_env_order_)++;
            std::shared_ptr<std::size_t> const& next_child_env_order
                = do_mark_child_env_as_forward_referenceable_ ? nullptr : next_child_env_order_;

            return root_shared_resource_->container.template allocate<Env>(
                base_env,
                forward_referenceable,
                decl_order,
                do_mark_child_env_as_forward_referenceable_,
                next_child_env_order,
                args... /*std::forward<Args>( args )...*/
                );
        }


        auto get_env_at( environment_id_t const& id )
            -> weak_env_base_pointer
        {
            return root_shared_resource_->container.at( id );
        }

        auto get_env_at( environment_id_t const& id ) const
            -> const_weak_env_base_pointer
        {
            return root_shared_resource_->container.at( id );
        }


        auto get_env_strong_at( environment_id_t const& id )
            -> env_base_pointer
        {
            return get_env_at( id ).lock();
        }

        auto get_env_strong_at( environment_id_t const& id ) const
            -> const_env_base_pointer
        {
            return get_env_at( id ).lock();
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
            kind::function_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> std::pair<
                     std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                     function_symbol_environment_ptr
                 >;

        auto mark_as(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> variable_symbol_environment_ptr;

        auto mark_as(
            kind::class_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> class_symbol_environment_ptr;

        auto mark_as(
            kind::template_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> std::pair<
                     template_set_environment_ptr,
                     template_environment_ptr
                 >;

        //
        // incomplete_construct
        //
        virtual auto incomplete_construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const&
            ) -> std::pair<
                     std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                     function_symbol_environment_ptr
                 >
        { assert( false ); return std::make_pair( nullptr, nullptr ); }

        virtual auto incomplete_construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&
            ) -> variable_symbol_environment_ptr
        { assert( false ); return nullptr; }

        virtual auto incomplete_construct(
            kind::class_tag,
            ast::identifier_value_base_ptr const&
            ) -> class_symbol_environment_ptr
        { assert( false ); return nullptr; }

        virtual auto incomplete_construct(
            kind::template_tag,
            ast::identifier_value_base_ptr const&
            ) -> std::pair<
                     template_set_environment_ptr,
                     template_environment_ptr
                 >
        { assert( false ); return std::make_pair( nullptr, nullptr ); }

        //
        // incomplete_construct
        //
        typedef std::function<function_symbol_environment_ptr (function_symbol_environment_ptr const&)> function_env_generator_scope_type;
        virtual auto construct(
            kind::function_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            function_env_generator_scope_type const&,
            class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> function_symbol_environment_ptr { assert( false ); return nullptr; }

        virtual auto construct(
            kind::variable_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&,
            const_class_symbol_environment_ptr const&,
            attribute::type_attributes const& = attribute::make_default_type_attributes()
            ) -> variable_symbol_environment_ptr { assert( false ); return nullptr; }

        virtual auto construct(
            kind::class_tag,
            ast::identifier_value_base_ptr const&,
            ast::statement_ptr const&
            ) -> class_symbol_environment_ptr { assert( false ); return nullptr; }




        //
        //
        //
        template<typename AstPtr>
        auto connect_from_ast( AstPtr const& ast )
            -> void
        {
            std::cout << "connect_from " << ast.get() << " -> : env " << get_id() << std::endl;
            //
            root_shared_resource_->ast_to_env_id_map.add( ast, shared_from_this() );
        }

        template<typename AstPtr>
        auto connect_to_ast( AstPtr const& ast )
            -> void
        {
            std::cout << "connect_to " << ast.get() << " -> : env " << get_id() << std::endl;
            //
            root_shared_resource_->env_id_to_ast_map.add( get_id(), ast );
        }

        template<typename AstPtr>
        auto link_with_ast( AstPtr const& ast )
            -> void
        {
            connect_from_ast( ast );
            connect_to_ast( ast );
        }

        auto get_related_ast()
            -> shared_resource_type::env_id_to_ast_mapper_type::value_type
        {
            return root_shared_resource_->env_id_to_ast_map.get( get_id() );
        }

        auto get_related_ast() const
            -> shared_resource_type::env_id_to_ast_mapper_type::const_value_type
        {
            // registered by connect_to_ast
            return root_shared_resource_->env_id_to_ast_map.get( get_id() );
        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr )
            -> env_base_pointer
        {
            auto const id = root_shared_resource_->ast_to_env_id_map.get( ast_ptr );

            return ( id != environment_id_undefined ) ? get_env_at( id ).lock() : env_base_pointer();
        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr ) const
            -> const_env_base_pointer
        {

            auto const id = root_shared_resource_->ast_to_env_id_map.get( ast_ptr );
            std::cout << "id: " << id << std::endl;
            return ( id != environment_id_undefined ) ? get_env_at( id ).lock() : const_env_base_pointer();
        }



        ///
        virtual auto mangled_name() const -> ast::native_string_t { return ""; }

        ///
        ///
        ///
        virtual auto dump( std::ostream& os, std::string const& indent ) const -> std::ostream& { return os; }




        //
        //
        //
        template<typename EnvPtrOrId>
        auto make_type_id(
            EnvPtrOrId const& e,
            attribute::type_attributes const& type_attr
            )
            -> shared_resource_type::type_registry_type::type_id_type
        {
            // TODO: DUPLICATE CHECK!!!
            return root_shared_resource_->types_container.add( e, type_attr );
        }

        auto get_type_at(
            shared_resource_type::type_registry_type::type_id_type const& type_id
            ) const
            -> shared_resource_type::type_registry_type::type_type
        {
            return root_shared_resource_->types_container.at( type_id );
        }

    private:
        environment_id_t id_;
        weak_env_base_pointer parent_;
        bool forward_referenceable_;
        std::size_t decl_order_;

    private:
        std::shared_ptr<shared_resource_type> root_shared_resource_;

        bool do_mark_child_env_as_forward_referenceable_;
        std::shared_ptr<std::size_t> next_child_env_order_;
    };

} // namespace rill
