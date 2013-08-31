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
#include <utility>
#include <boost/range/adaptor/transformed.hpp>

#include <boost/algorithm/string/join.hpp>

//#include <boost/detail/bitmask.hpp>
//#include <boost/optional.hpp>

#include "config/macros.hpp"

#include "environment_fwd.hpp"

#include "ast/value.hpp"
#include "ast/expression.hpp"
#include "ast/statement.hpp"

using namespace rill::ast;  // TODO: fix


typedef unsigned int    symbol_types_mask_t;
enum struct symbol_types : symbol_types_mask_t
{
    root_c = ( 1u << 0 )
};




enum struct symbol_kind
{
    variable_k,
    function_k,
    type_k,
    namespace_k
};



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


template<>
struct kind_classifier<function_definition_statement_base_ptr>
{
    static const symbol_kind value = symbol_kind::function_k;
};


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





#include <vector>
template<typename BaseEnvT>
class environment_resource
{
public:
    template<typename Env>
    struct result
    {
        environment_id_t id;
        std::shared_ptr<Env> pointer;
    };

    //
    template<typename Env, typename... T>
    auto allocate( T&&... ts )
        -> result<Env>
    {
        environment_id_t const next_id = nodes_.size();
        if ( next_id == environment_id_limit )
            exit( -999 );

        auto const p = std::make_shared<Env>( next_id, std::forward<T>( ts )... );
        result<Env> r = { next_id, p };

        nodes_.push_back( p );

        return r;
    }

    //
    auto at( environment_id_t const& id )
        -> std::weak_ptr<BaseEnvT>
    {
        assert( id >= 0 && id < nodes_.size() );
        return nodes_.at( id );
    }

    //
    auto at( environment_id_t const& id ) const
        -> std::weak_ptr<BaseEnvT const>
    {
        assert( id >= 0 && id < nodes_.size() );
        return nodes_.at( id );
    }

private:
    std::vector<std::weak_ptr<BaseEnvT>> nodes_;
};


struct root_initialize_tag {};








class environment
    : public std::enable_shared_from_this<environment>
{
public:
    typedef environment                         env_type;

    typedef std::shared_ptr<env_type>           env_pointer;
    typedef std::shared_ptr<env_type const>     const_env_pointer;
    typedef std::weak_ptr<env_type>             weak_env_pointer;
    typedef std::weak_ptr<env_type const>       const_weak_env_pointer;

    typedef native_string_t                     native_string_type;

public:
    environment( root_initialize_tag )
        : id_( envitonment_id_undefined )
        , shared_env_memory_( std::make_shared<environment_resource<env_type>>() )
    {
        std::cout << ">> environment allocated" << std::endl;
    }
    
    environment( environment_id_t const& id, weak_env_pointer const& parent )
        : id_( id )
        , parent_( parent )
        , shared_env_memory_( parent.lock()->shared_env_memory_ )
    {
        std::cout << ">> environment allocated(inner)" << std::endl;
    }

    virtual ~environment()
    {
        std::cout << "<< environment DEallocated" << std::endl;
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
    virtual auto pre_construct(
        kind::function_tag,
        intrinsic::single_identifier_value_base_ptr const&
        ) -> env_pointer { assert( false ); return nullptr; }

    typedef std::function<function_symbol_environment_ptr (function_symbol_environment_ptr const&)> function_env_generator_scope_type;
    virtual auto construct(
        kind::function_tag,
        intrinsic::single_identifier_value_base_ptr const& name,
        function_env_generator_scope_type const& parameter_decl_initializer,
        statement_list const& statements
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
        -> typename environment_resource<env_type>::result<Env>
    {
        return shared_env_memory_->allocate<Env>( /*std::forward<Args>( args )...*/ args... );
    }


    auto get_env_at( environment_id_t const& id )
        -> weak_env_pointer
    {
        return shared_env_memory_->at( id );
    }

    auto get_env_at( environment_id_t const& id ) const
        -> const_weak_env_pointer
    {
        return shared_env_memory_->at( id );
    }

    auto get_id() const
        -> environment_id_t
    {
        return id_;
    }

    virtual auto is_root() const -> bool { return false; }
    auto get_parent_env() -> env_pointer { return is_root() ? nullptr : parent_.lock(); }
    auto get_parent_env() const -> const_env_pointer { return is_root() ? nullptr : parent_.lock(); }



    ///
    ///
    ///
    virtual auto dump( std::ostream& os, std::string const& indent ) const -> std::ostream& { return os; }


private:
    environment_id_t id_;
    weak_env_pointer parent_;

    std::shared_ptr<environment_resource<env_type>> shared_env_memory_;
};


std::ostream& operator<<( std::ostream& os, environment_ptr const& env );






class template_environment RILL_CXX11_FINAL
    : public environment
{
    template<typename T>
    friend class environment_allocator;

public:
    template_environment( environment_id_t const& id, weak_env_pointer const& parent, void* )
        : environment( id, parent )
    {}

public:
    auto get_symbol_kind() const
        -> kind::type_value RILL_CXX11_OVERRIDE
    {
        return kind::type_value::none_e; // TODO: change to template_e
    }

private:
    std::unordered_map<native_string_type, environment_ptr> simple_env_;
};



//
class single_identifier_environment_base
    : public environment
{
    typedef single_identifier_environment_base      self_type;
    typedef std::shared_ptr<self_type>              self_pointer;
    typedef std::shared_ptr<self_type>              self_const_pointer;

public:
    single_identifier_environment_base( root_initialize_tag )
        : environment( root_initialize_tag() )
    {}
    
    single_identifier_environment_base( environment_id_t const& id, weak_env_pointer const& parent )
        : environment( id, parent )
    {}

    virtual ~single_identifier_environment_base() {};

public:
    auto lookup( intrinsic::const_single_identifier_value_base_ptr const& )
        -> env_pointer RILL_CXX11_OVERRIDE;
    auto lookup( intrinsic::const_single_identifier_value_base_ptr const& ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE;

    //
    auto find_on_env( intrinsic::const_single_identifier_value_base_ptr const& )
        -> env_pointer RILL_CXX11_OVERRIDE;
    auto find_on_env( intrinsic::const_single_identifier_value_base_ptr const& ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE;





    //
    virtual auto is_incomplete() const
        -> bool { return false; }


/*
    auto is_exist_in_instanced( native_string_type const& name ) const
        -> boost::optional<env_pointer>
    {
        auto const& it = instanced_env_.find( name );
        if ( it != instanced_env_.cend() )
            return it->second;

        return boost::none;
    }*/

    /*
    auto is_same_pre_declared_type( intrinsic::identifier_value_ptr const& name, symbol_kind const& kind ) const
        -> bool
    {

    }*/

    // function
    auto pre_construct(
        kind::function_tag,
        intrinsic::single_identifier_value_base_ptr const&
        )  -> env_pointer RILL_CXX11_OVERRIDE;
    /*
    auto construct(
        kind::function_tag,
        intrinsic::single_identifier_value_base_ptr const&,
        parameter_list const&,
        intrinsic::identifier_value_ptr const&,
        statement_list const&
        ) -> env_pointer RILL_CXX11_OVERRIDE;
        */
    auto construct(
        kind::function_tag,
        intrinsic::single_identifier_value_base_ptr const& name,
        function_env_generator_scope_type const& parameter_decl_initializer,
        statement_list const& statements
        ) -> function_symbol_environment_ptr RILL_CXX11_OVERRIDE;

    // variable(decl)
    auto construct(
        kind::variable_tag,
        intrinsic::single_identifier_value_base_ptr const&,
        const_class_symbol_environment_ptr const&
        ) -> variable_symbol_environment_ptr RILL_CXX11_OVERRIDE;

    // class(type)
    auto pre_construct(
        kind::class_tag,
        intrinsic::single_identifier_value_ptr const& name
        ) -> env_pointer RILL_CXX11_OVERRIDE;

    auto construct(
        kind::class_tag,
        intrinsic::single_identifier_value_base_ptr const& name
        ) -> class_symbol_environment_ptr RILL_CXX11_OVERRIDE;
    /*
    auto pre_construct( kind::class_tag, intrinsic::identifier_value_ptr const& name ) RILL_CXX11_OVERRIDE
        -> env_pointer;
    {
    }*/

    auto dump( std::ostream& os, std::string const& indent ) const
        -> std::ostream& RILL_CXX11_OVERRIDE
    {
        os  << indent << "single_identifier_environment_base" << std::endl;
        return dump_include_env( os, indent );
    }

    auto dump_include_env( std::ostream& os, std::string const& indent ) const
        -> std::ostream&
    {
        for( auto const& ins : instanced_env_ ) {
            os << indent
               << "-> symbol: " << ins.first
               << " / id: " << ins.second->get_id()
               << " / symbol kind: " << static_cast<int>( ins.second->get_symbol_kind() ) << std::endl;
        }

        return os;
    }


private:
    std::unordered_map<native_string_type, env_pointer> instanced_env_;
    std::unordered_map<native_string_type, std::shared_ptr<template_environment>> template_env_;
};




// for root
class root_environment RILL_CXX11_FINAL
    : public single_identifier_environment_base
{
public:
    root_environment()
        : single_identifier_environment_base( root_initialize_tag() )
    {}

private:
    auto get_symbol_kind() const
        -> kind::type_value RILL_CXX11_OVERRIDE
    {
        return kind::type_value::none_e;
    }

    bool is_root() const RILL_CXX11_OVERRIDE
    {
        return true;
    }

private:

};








class has_parameter_environment_base
    : public environment
{
public:
    has_parameter_environment_base( environment_id_t const id, weak_env_pointer const& parent )
        : environment( id, parent )
    {}

    virtual ~has_parameter_environment_base() {}

public:
    virtual auto get_inner_symbol_kind() const
        -> kind::type_value =0;
 /*
    virtual auto add_overload( parameter_list const& parameter, function_definition_statement_base_ptr const& sp* )
        -> env_pointer =0;
   
    virtual auto lookup( environment_ptr const& parent, parameter_list const& parameter ) const
        -> const_environment_ptr =0;*/
};


typedef std::string parameter_hash_t;

template<typename EnvIds>
inline auto make_parameter_hash( EnvIds const& id_list )
    -> parameter_hash_t
{
    {
        // debu:
        for( auto const& i : id_list )
            std::cout << ":" << i << " ";
        std::cout << std::endl;
    }

    return  std::to_string( id_list.size() )
            + "_"
            + boost::algorithm::join(
                id_list
                | boost::adaptors::transformed(
                    std::function<std::string (environment_id_t const&)>( []( environment_id_t const& id ){ return std::to_string( id ); } )
                    ),
                "%"
                );
}



// parameter_environment has list of class(type)_environments
// it makes be able to overload
// picked the type matched environments when look up

// TODO: change to -> typedef std::vector<const_class_symbol_environment_ptr> type_environment_list;
typedef std::vector<const_environment_ptr> type_environment_list;


template<typename InlineEnvironment>
class has_parameter_environment RILL_CXX11_FINAL
    : public has_parameter_environment_base
{
public:
    static kind::type_value const KindValue = InlineEnvironment::KindValue;

public:
    has_parameter_environment( environment_id_t const id, weak_env_pointer const& parent )
        : has_parameter_environment_base( id, parent )
    {}

public:
    auto get_symbol_kind() const
        -> kind::type_value RILL_CXX11_OVERRIDE
    {
        return kind::type_value::parameter_wrapper_e;
    }

    auto get_inner_symbol_kind() const
        -> kind::type_value  RILL_CXX11_OVERRIDE
    {
        return KindValue;
    }

    template<typename... Args>
    auto allocate_inner_env( Args&&... args )
        -> std::shared_ptr<InlineEnvironment>
    {
        // parant environment is not this env but one rank top env
        return allocate_env<InlineEnvironment>( get_parent_env(), std::forward<Args>( args )... ).pointer;
    }


    auto add_overload( std::shared_ptr<InlineEnvironment> const& inner_env )
        -> env_pointer
    {
        // TODO: add duplicate check
        overloads_[make_parameter_hash( inner_env->get_arg_load_env_ids() )] = inner_env;

        return inner_env;
    }

    auto solve_overload( environment_id_list const& args_env_ids ) const
        -> std::shared_ptr<InlineEnvironment>
    {
//        std::cout << "solve_overload? hash: " << make_parameter_hash( args_env_ids ) << std::endl;

        auto const it = overloads_.find( make_parameter_hash( args_env_ids ) );

        return it != overloads_.cend() ? it->second : nullptr;
    }

    // delegate lookup
    auto lookup( intrinsic::const_single_identifier_value_base_ptr const& name )
        -> env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->lookup( name ); }
    auto lookup( intrinsic::const_single_identifier_value_base_ptr const& name ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->lookup( name ); }
    auto find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name )
        -> env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->find_on_env( name ); }
    auto find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->find_on_env( name ); }
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
               << indent << (environment_ptr const&)p.second << std::endl
               << indent << "======" << std::endl;
        }
        return os;
    }

private:
    std::unordered_map<parameter_hash_t, std::shared_ptr<InlineEnvironment>> overloads_;
};









//
// function
//
class function_symbol_environment RILL_CXX11_FINAL
    : public single_identifier_environment_base
{
public:
    static kind::type_value const KindValue = kind::type_value::function_e;

public:
    function_symbol_environment( environment_id_t const& id, weak_env_pointer const& parent, statement_list const& statements )
        : single_identifier_environment_base( id, parent )
        , statements_( statements )
    {}

public:
    auto get_symbol_kind() const
        -> kind::type_value RILL_CXX11_OVERRIDE
    {
        return KindValue;
    }

    auto is_incomplete() const
        -> bool RILL_CXX11_OVERRIDE
    {
        return false;
    }

    auto get_statement_list() const
        -> statement_list const&
    {
        return statements_;
    }

    auto get_arg_load_env_ids() const
        -> std::vector<environment_id_t> const&
    {
        return parameter_type_ids_;
    }

    auto complete( const_environment_ptr const& return_type_env )
        -> void
    {
        return_type_env_id_ = return_type_env->get_id();
    }

    auto get_return_type_environment()
        -> class_symbol_environment_ptr
    {
        auto const& p = get_env_at( return_type_env_id_ );

        return std::dynamic_pointer_cast<class_symbol_environment>( p.lock() );
    }

    auto dump( std::ostream& os, std::string const& indent ) const
        -> std::ostream& RILL_CXX11_OVERRIDE
    {
        os  << indent << "function_symbol_environment" << std::endl;
        return dump_include_env( os, indent );
    }

    auto parameter_variable_construct(
        /* ,*/
        intrinsic::single_identifier_value_base_ptr const& name,
        const_class_symbol_environment_ptr const& type_env
        )
        -> variable_symbol_environment_ptr;

private:
    std::vector<environment_id_t> parameter_decl_ids_, parameter_type_ids_;
    environment_id_t return_type_env_id_;

    statement_list statements_;
};
//typedef std::shared_ptr<function_symbol_environment>        function_symbol_environment_ptr;
//typedef std::shared_ptr<function_symbol_environment const>  const_function_symbol_environment_ptr;




//
// variable
//
class variable_symbol_environment RILL_CXX11_FINAL
    : public single_identifier_environment_base
{
public:
    static kind::type_value const KindValue = kind::type_value::variable_e;

public:
    variable_symbol_environment( environment_id_t const& id, weak_env_pointer const& parent )
        : single_identifier_environment_base( id, parent )
//        , value_type_env_id_( envitonment_id_undefined )
    {}

public:
    auto get_symbol_kind() const
        -> kind::type_value RILL_CXX11_OVERRIDE
    {
        return KindValue;
    }

    auto is_incomplete() const
        -> bool RILL_CXX11_OVERRIDE
    {
        return true;//value_type_env_id_ == envitonment_id_undefined;
    }

/*    auto get_weak_type_env()
        -> weak_environment_ptr
    {
        return get_env_at( value_type_env_id_ );
    }
    auto get_weak_type_env() const
        -> const_weak_environment_ptr
    {
        return get_env_at( value_type_env_id_ );
    }*/

    auto dump( std::ostream& os, std::string const& indent ) const
        -> std::ostream& RILL_CXX11_OVERRIDE
    {
        os << indent << "varialbe_environment" << std::endl;
//        os << indent << "  = return type  " << value_type_env_id_ << std::endl;
        return dump_include_env( os, indent );
    }

private:
//    environment_id_t value_type_env_id_;
};
//typedef std::shared_ptr<variable_symbol_environment>        variable_symbol_environment_ptr;
//typedef std::shared_ptr<variable_symbol_environment const>  const_variable_symbol_environment_ptr;



//
// class(type)
//
class class_symbol_environment RILL_CXX11_FINAL
    : public single_identifier_environment_base
{
public:
    static kind::type_value const KindValue = kind::type_value::class_e;

public:
    class_symbol_environment( environment_id_t const& id, weak_env_pointer const& parent )
        : single_identifier_environment_base( id, parent )
        //, kind_( kind )
    {}

public:
    auto get_symbol_kind() const
        -> kind::type_value RILL_CXX11_OVERRIDE
    {
        return KindValue;
    }

    auto is_incomplete() const
        -> bool RILL_CXX11_OVERRIDE
    {
        return true;
    }

    auto dump( std::ostream& os, std::string const& indent ) const
        -> std::ostream& RILL_CXX11_OVERRIDE
    {
        os  << indent << "class_symbol_environment" << std::endl;
        return dump_include_env( os, indent );
    }

private:

    //statement_list sp_;
    //symbol_kind kind_;
};
//typedef std::shared_ptr<class_symbol_environment>        class_symbol_environment_ptr;
//typedef std::shared_ptr<class_symbol_environment const>  const_class_symbol_environment_ptr;
