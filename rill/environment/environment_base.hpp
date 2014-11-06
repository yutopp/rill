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
        : public std::enable_shared_from_this<environment_base>
    {
    public:
        environment_unit( environment_parameter_t&& ep )
            : b_( ep.global_env )
            , id_( ep.id )
            , parent_( ep.parent )
            , owner_module_id_( ep.mod_id )
            , is_private_( ep.is_private )
        {
            std::cout << ">> environment constructed(as a child)"
                      << " / id: " << id_ << std::endl;
        }

        virtual ~environment_unit()
        {
            std::cout << "<< environment destructed / id: " << id_ << " = " << typeid(*this).name() << std::endl;
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


            inline auto cast_to_base( std::shared_ptr<environment_unit> p )
            -> environment_base_ptr
        {
            assert( p != nullptr );
            assert( p->has_elements() == true );
            return std::static_pointer_cast<environment_base>( p );
        }

        inline auto cast_to_base( std::shared_ptr<environment_unit const> p )
            -> const_environment_base_ptr
        {
            assert( p != nullptr );
            assert( p->has_elements() == true );
            return std::static_pointer_cast<environment_base const>( p );
        }

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
            -> env_base_pointer
        {
            return cast_to_base( environment_unit::get_parent_env() );
        }

        auto get_parent_env() const
            -> const_env_base_pointer
        {
            return cast_to_base( environment_unit::get_parent_env() );
        }

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

        auto lookup_layer( kind::type_value const& layer_type ) const
            -> const_env_base_pointer
        {
            auto p = shared_from_this();
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

        auto root_env()
            -> env_base_pointer
        {
            auto p = shared_from_this();
            while( !p->is_root() ) {
                p = p->get_parent_env();
            }

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

        auto import_from( const_environment_base_ptr const& )
            -> void;







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
        environment_process_progress_t progress_;
        std::unordered_map<native_string_type, environment_unit_ptr> inner_envs_;   // children environments
    };

} // namespace rill
