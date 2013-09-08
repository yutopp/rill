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

#include "../environment_fwd.hpp" //FIXit
#include "detail/container.hpp"
#include "detail/mapper.hpp"

#include "../ast/value.hpp"
#include "../ast/expression.hpp"
#include "../ast/statement.hpp"


namespace rill
{
    using namespace rill::ast;  // TODO: fix


    typedef unsigned int    symbol_types_mask_t;
    enum struct symbol_types : symbol_types_mask_t
    {
        root_c = ( 1u << 0 )
    };



    /*
    enum struct symbol_kind
    {
        variable_k,
        function_k,
        type_k,
        namespace_k
    };
    */


    struct common_spec
    {
        enum
        {
            unique_symbol = ( 1 << 0 )
        };
    };
    std::size_t const common_spec_num = 1;
    typedef std::bitset<common_spec_num> common_spec_flags_t;


    namespace kind
    {
        struct function_tag {};
        auto const function_k = function_tag();

        struct class_tag {};
        auto const class_k = class_tag();

        struct variable_tag {};
        auto const variable_k = variable_tag();

        enum struct type_value
        {
            none_e,
            function_e,
            parameter_wrapper_e,
            variable_e,
            class_e
        };
    }

    template<typename>
    struct kind_classifier;





    enum struct error_code
    {
    };


    enum struct length_type
    {
        fixed,
        variable
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
        typedef typename environment_container<BaseEnvT>    env_container_type;
        typedef ast_to_environment_id_mapper                ast_to_env_id_mapper_type;
        typedef environment_id_to_ast_mapper                env_id_to_ast_mapper_type;

        env_container_type container;
        ast_to_env_id_mapper_type ast_to_env_id_map;
        env_id_to_ast_mapper_type env_id_to_ast_map;

        debug_allocate_counter debug_allocate_counter_;
    };




    struct root_initialize_tag {};








    class environment
        : public std::enable_shared_from_this<environment>
    {
    public:
        typedef environment                             env_type;

        typedef std::shared_ptr<env_type>               env_pointer;
        typedef std::shared_ptr<env_type const>         const_env_pointer;
        typedef std::weak_ptr<env_type>                 weak_env_pointer;
        typedef std::weak_ptr<env_type const>           const_weak_env_pointer;

        typedef native_string_t                         native_string_type;
        typedef environment_shared_resource<env_type>   shared_resource_type;

    public:
        // construct as ROOT environment
        environment( root_initialize_tag )
            : id_( environment_id_undefined )
            , root_shared_resource_( std::make_shared<environment_shared_resource<env_type>>() )
        {
            std::cout << ">> environment allocated" << " ( "  << root_shared_resource_->debug_allocate_counter_.value <<" )" << std::endl;

            ++root_shared_resource_->debug_allocate_counter_.value;
        }

        // normal constructor
        environment( environment_id_t const& id, weak_env_pointer const& parent )
            : id_( id )
            , parent_( parent )
            , root_shared_resource_( parent.lock()->root_shared_resource_ )
        {
            std::cout << ">> environment allocated(inner): " << id_ << " ( "  << root_shared_resource_->debug_allocate_counter_.value <<" )"  << std::endl;

            ++root_shared_resource_->debug_allocate_counter_.value;
        }

        virtual ~environment()
        {
            --root_shared_resource_->debug_allocate_counter_.value;

            std::cout << "<< environment DEallocated: " << id_ << " ( "  << root_shared_resource_->debug_allocate_counter_.value <<" )"  << std::endl;
        }

    public:
        //
        virtual auto lookup( intrinsic::const_single_identifier_value_base_ptr const& name )
            -> env_pointer =0;
        virtual auto lookup( intrinsic::const_single_identifier_value_base_ptr const& name ) const
            -> const_env_pointer =0;

        //
        virtual auto find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name )
            -> env_pointer =0;
        virtual auto find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name ) const
            -> const_env_pointer =0;

        //
        template<typename F>
        auto nest_lookup( intrinsic::const_identifier_value_ptr const& ids, F const& failed_callback )
            -> env_pointer
        {
            // current environment
            env_pointer env = shared_from_this();

            for( auto const& id : ids->nest_ ) {
                auto const& temp_env = env;
                if ( env == shared_from_this() ) {
                    env = env->lookup( id );
                } else {
                    env = env->find_on_env( id );
                }

                if ( env == nullptr ) {
                    env = failed_callback( temp_env, id );
                    if ( env == nullptr )
                        break;
                }
            }
            return env;
        }

        auto nest_lookup( intrinsic::const_identifier_value_ptr const& ids )
            -> env_pointer
        {
            return nest_lookup( ids, []( env_pointer const&, intrinsic::const_single_identifier_value_base_ptr const& ){ return nullptr; } );
        }

        template<typename F>
        auto nest_lookup( intrinsic::const_identifier_value_ptr const& ids ) const
            -> const_env_pointer
        {
            const_env_pointer env = shared_from_this();

            for( auto const& id : ids->nest_ ) {
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




        // function
        virtual auto incomplete_construct(
            kind::function_tag,
            intrinsic::single_identifier_value_base_ptr const& name
            ) -> std::pair<
                    std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                    function_symbol_environment_ptr
               >
        {
            assert( false );
            return std::make_pair( nullptr, nullptr );
        }



        typedef std::function<function_symbol_environment_ptr (function_symbol_environment_ptr const&)> function_env_generator_scope_type;
        virtual auto construct(
            kind::function_tag,
            intrinsic::single_identifier_value_base_ptr const& name,
            function_env_generator_scope_type const& parameter_decl_initializer,
            class_symbol_environment_ptr const& return_type_env,
            ast::statement_ptr const& ast
            ) -> function_symbol_environment_ptr { assert( false ); return nullptr; }
        /*virtual auto construct(
            kind::function_tag,
            intrinsic::single_identifier_value_base_ptr const&,
            parameter_list const&,
            intrinsic::identifier_value_ptr const&,
            statement_list const&
            ) -> env_pointer { assert( false ); return nullptr; }*/

        // variable
        virtual auto construct(
            kind::variable_tag,
            intrinsic::single_identifier_value_base_ptr const&,
            const_class_symbol_environment_ptr const&
            ) -> variable_symbol_environment_ptr { assert( false ); return nullptr; }

        // class
        virtual auto pre_construct(
            kind::class_tag,
            intrinsic::single_identifier_value_ptr const&
            )  -> env_pointer { assert( false ); return nullptr; }

        virtual auto construct(
            kind::class_tag,
            intrinsic::single_identifier_value_base_ptr const&
            ) -> class_symbol_environment_ptr { assert( false ); return nullptr; }

        //
        virtual auto get_symbol_kind() const
            -> kind::type_value =0;

        auto root_env()
            -> env_pointer
        {
            auto p = shared_from_this();
            while( !p->is_root() )
                p = p->get_parent_env();

            return p;
        }

        auto root_env() const
            -> const_env_pointer
        {
            auto p = shared_from_this();
            while( !p->is_root() )
                p = p->get_parent_env();

            return p;
        }

        auto lookup_on_root( intrinsic::const_single_identifier_value_ptr const& type_name )
            -> env_pointer
        {
            return root_env()->lookup( type_name );
        }
        auto lookup_on_root( intrinsic::const_single_identifier_value_ptr const& type_name ) const
            -> const_env_pointer
        {
            return root_env()->lookup( type_name );
        }




        template<typename Env, typename... Args>
        auto allocate_env( Args&&... args )
            -> typename environment_container<env_type>::template result<Env>::type
        {
            return root_shared_resource_->container.allocate<Env>( /*std::forward<Args>( args )...*/ args... );
        }


        auto get_env_at( environment_id_t const& id )
            -> weak_env_pointer
        {
            return root_shared_resource_->container.at( id );
        }

        auto get_env_at( environment_id_t const& id ) const
            -> const_weak_env_pointer
        {
            return root_shared_resource_->container.at( id );
        }

        auto get_id() const
            -> environment_id_t
        {
            return id_;
        }


        virtual auto is_root() const -> bool { return false; }
        auto get_parent_env() -> env_pointer { return is_root() ? nullptr : parent_.lock(); }
        auto get_parent_env() const -> const_env_pointer { return is_root() ? nullptr : parent_.lock(); }

        template<typename AstPtr>
        auto mark_as( kind::function_tag, intrinsic::single_identifier_value_base_ptr const& name_identifier, AstPtr const& ast )
            -> decltype( incomplete_construct( kind::function_tag(), name_identifier ) )
        {
            // construct incomplete environment( parameter wrapper & function )
            auto const p = incomplete_construct( kind::function_tag(), name_identifier );
            auto const& has_parameter_env = p.first;
            auto const& created_function_env = p.second;

            std::cout << "%&%& " << has_parameter_env->get_id() << " : " << created_function_env->get_id() << std::endl;

            //
            root_shared_resource_->env_id_to_ast_map.add( has_parameter_env->get_id(), ast );     // 
            root_shared_resource_->env_id_to_ast_map.add( created_function_env->get_id(), ast );                    // related environment of created_function_env is parent envitroment of it

            //
            root_shared_resource_->ast_to_env_id_map.add( ast, created_function_env->get_id() );

            return p;
        }

#if 0
        auto get_related_env_and_asts() const
            -> boost::iterator_range<shared_resource_type::env_to_asts_mapper_type::const_iterator_type>
        {
            auto const& p = root_shared_resource_->env_to_asts_map.get( get_id() );
            return boost::make_iterator_range( p.first, p.second );
        }
#endif
        auto get_related_ast() const
            -> shared_resource_type::env_id_to_ast_mapper_type::value_type
        {
            return root_shared_resource_->env_id_to_ast_map.get( get_id() );
        }

        template<typename AstPtr>
        auto get_related_env_by_ast_ptr( AstPtr const& ast_ptr )
            -> env_pointer
        {
            return get_env_at( root_shared_resource_->ast_to_env_id_map.get( ast_ptr ) ).lock();
        }

        ///
        ///
        ///
        virtual auto dump( std::ostream& os, std::string const& indent ) const -> std::ostream& { return os; }


    private:
        environment_id_t id_;
        weak_env_pointer parent_;

        std::shared_ptr<shared_resource_type> root_shared_resource_;
    };


    std::ostream& operator<<( std::ostream& os, environment_ptr const& env );


} // namespace rill
