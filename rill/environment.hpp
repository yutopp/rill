//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

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

#include "value.hpp"
#include "expression.hpp"
#include "statement.hpp"

typedef unsigned int    symbol_types_mask_t;
enum struct symbol_types : symbol_types_mask_t
{
    root_c = ( 1u << 0 )
};


class value_type_table_environment;
class function_type_table_environment;


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

    enum struct type_value
    {
        none_e,
        function_e,
        parameter_wrapper_e,
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





#include <vector>
template<typename BaseEnvT>
class environment_allocator
{
public:
    template<typename Env>
    struct result
    {
        environment_id_t id;
        std::shared_ptr<Env> pointer;
    };

    template<typename Env, typename... T>
    auto allocate( T&&... ts )
        -> result<Env>
    {
        environment_id_t const next_id = nodes_.size();
        if ( next_id == environment_id_limit )
            exit( -999 );

        auto const p = std::make_shared<Env>( next_id, std::forward<T>( ts )... );
        nodes_.push_back( p );

        result<Env> r = { next_id, p };
        return r;
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

    typedef native_string_t                     native_string_type;

public:
    environment( root_initialize_tag )
        : id_( envitonment_id_undefined )
        , managed_( std::make_shared<environment_allocator<env_type>>() )
    {
//        std::cout << ">> environment allocated" << std::endl;
    }
    
    environment( environment_id_t const& id, weak_env_pointer const& parent )
        : id_( id )
        , parent_( parent )
        , managed_( parent.lock()->managed_ )
    {
//        std::cout << ">> environment allocated(inner)" << std::endl;
    }

    ~environment()
    {
//        std::cout << "<< environment DEallocated" << std::endl;
    }

public:
    //
    virtual auto lookup( literal::const_single_identifier_value_base_ptr const& name )
        -> env_pointer =0;
    virtual auto lookup( literal::const_single_identifier_value_base_ptr const& name ) const
        -> const_env_pointer =0;

    //
    virtual auto find_on_env( literal::const_single_identifier_value_base_ptr const& name )
        -> env_pointer =0;
    virtual auto find_on_env( literal::const_single_identifier_value_base_ptr const& name ) const
        -> const_env_pointer =0;

    template<typename F>
    auto nest_lookup( literal::identifier_value_ptr const& ids, F const& failed_callback )
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

    auto nest_lookup( literal::identifier_value_ptr const& ids )
        -> env_pointer
    {
        return nest_lookup( ids, []( env_pointer const&, literal::single_identifier_value_base_ptr const& ){ return nullptr; } );
    }

    template<typename F>
    auto nest_lookup( literal::identifier_value_ptr const& ids ) const
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





    //
    virtual auto pre_construct(
        kind::function_tag,
        literal::single_identifier_value_base_ptr const&
        ) -> env_pointer { return nullptr; }

    virtual auto construct(
        kind::function_tag,
        literal::single_identifier_value_base_ptr const&,
        parameter_list const&,
        statement_list const&
        ) -> env_pointer { return nullptr; }

    virtual auto pre_construct(
        kind::class_tag,
        literal::single_identifier_value_ptr const&
        )  -> env_pointer { return nullptr; }

    virtual auto construct(
        kind::class_tag,
        literal::single_identifier_value_base_ptr const&
        ) -> env_pointer { return nullptr; }

    virtual auto get_symbol_kind() const
        -> kind::type_value =0;
    
    auto root_env() const
        -> const_env_pointer
    {
        auto p = shared_from_this();
        while( !p->is_root() )
            p = p->get_parent_env();

        return p;
    }

    auto lookup_env_on_root( literal::const_single_identifier_value_ptr const& type_name ) const
        -> const_env_pointer
    {
        return root_env()->lookup( type_name );
    }

    template<typename Env, typename... Args>
    auto allocate_env( Args&&... args )
        -> typename environment_allocator<env_type>::result<Env>
    {
        return managed_->allocate<Env>( /*std::forward<Args>( args )...*/ args... );
    }

    auto get_id() const
        -> environment_id_t
    {
        return id_;
    }

    virtual auto is_root() const -> bool { return false; }
    auto get_parent_env() -> env_pointer { return is_root() ? nullptr : parent_.lock(); }
    auto get_parent_env() const -> const_env_pointer { return is_root() ? nullptr : parent_.lock(); }

    virtual auto dump( std::ostream& os, std::string const& indent ) const -> std::ostream& { return os; }


private:
    environment_id_t id_;
    weak_env_pointer parent_;

    std::shared_ptr<environment_allocator<env_type>> managed_;
};
//typedef environment::self_pointer           environment_ptr;
//typedef environment::const_self_pointer     const_environment_ptr;
//typedef environment::weak_self_pointer      weak_environment_ptr;


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

    // not implemented
    auto lookup( literal::const_single_identifier_value_base_ptr const& )
        -> env_pointer RILL_CXX11_OVERRIDE { assert( false ); return nullptr; }
    auto lookup( literal::const_single_identifier_value_base_ptr const& ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE { assert( false ); return nullptr; }
    auto find_on_env( literal::const_single_identifier_value_base_ptr const& )
        -> env_pointer RILL_CXX11_OVERRIDE { assert( false ); return nullptr; }
    auto find_on_env( literal::const_single_identifier_value_base_ptr const& ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE { assert( false ); return nullptr; }

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
    auto lookup( literal::const_single_identifier_value_base_ptr const& )
        -> env_pointer RILL_CXX11_OVERRIDE;
    auto lookup( literal::const_single_identifier_value_base_ptr const& ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE;

    //
    auto find_on_env( literal::const_single_identifier_value_base_ptr const& )
        -> env_pointer RILL_CXX11_OVERRIDE;
    auto find_on_env( literal::const_single_identifier_value_base_ptr const& ) const
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
    auto is_same_pre_declared_type( literal::identifier_value_ptr const& name, symbol_kind const& kind ) const
        -> bool
    {

    }*/

    auto pre_construct(
        kind::function_tag,
        literal::single_identifier_value_base_ptr const& name
        )  -> env_pointer RILL_CXX11_OVERRIDE;

    auto construct(
        kind::function_tag,
        literal::single_identifier_value_base_ptr const& name,
        parameter_list const&,
        statement_list const& statements
        ) -> env_pointer RILL_CXX11_OVERRIDE;


    auto pre_construct(
        kind::class_tag,
        literal::single_identifier_value_ptr const& name
        ) -> env_pointer RILL_CXX11_OVERRIDE;

    auto construct(
        kind::class_tag,
        literal::single_identifier_value_base_ptr const& name
        ) -> env_pointer RILL_CXX11_OVERRIDE;
    /*
    auto pre_construct( kind::class_tag, literal::identifier_value_ptr const& name ) RILL_CXX11_OVERRIDE
        -> env_pointer;
    {
    }*/

    auto dump( std::ostream& os, std::string const& indent ) const
        -> std::ostream& RILL_CXX11_OVERRIDE
    {
        os  << indent << "single_identifier_environment_base" << std::endl;
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







class function_symbol_environment;
typedef std::shared_ptr<function_symbol_environment>        function_symbol_environment_ptr;
typedef std::shared_ptr<function_symbol_environment const>  const_function_symbol_environment_ptr;

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

private:
    statement_list statements_;
};



class has_parameter_environment_base
    : public environment
{
public:
    has_parameter_environment_base( environment_id_t const id, weak_env_pointer const& parent )
        : environment( id, parent )
    {}

public:
    virtual auto get_inner_symbol_kind() const
        -> kind::type_value =0;
 /*
    virtual auto add_overload( parameter_list const& parameter/*, function_definition_statement_base_ptr const& sp* )
        -> env_pointer =0;
   
    virtual auto lookup( environment_ptr const& parent, parameter_list const& parameter ) const
        -> const_environment_ptr =0;*/
};


typedef std::string parameter_hash_t;

template<typename EnvIds>
inline auto make_parameter_hash( EnvIds const& id_list )
    -> parameter_hash_t
{
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

    auto add_overload( parameter_list const& parameter, statement_list const& statements )
        -> env_pointer
    {
        auto const ns_range
            = parameter
            | boost::adaptors::transformed(
                std::function<const_env_pointer (parameter_pair const&)>(
                    [this]( parameter_pair const& pp ) {
                        return this->nest_lookup( pp.type );
                    } )
                )
            ;

        for( auto const& ns : ns_range ) {
            if ( ns == nullptr )
                return nullptr;
        }

        auto const env_ids_range
            = ns_range
            | boost::adaptors::transformed(
                std::function<environment_id_t (const_env_pointer const&)>(
                    []( const_env_pointer const& e ) {
                        return e->get_id();
                    } )
                )
            ;

        auto const& f = allocate_env<InlineEnvironment>( get_parent_env(), statements ).pointer;

        // TODO: add duplicate check
        overloads_[make_parameter_hash( env_ids_range )] = f;

        return f;
    }

    auto solve_overload( environment_id_list const& args_env_ids ) const
        -> std::shared_ptr<InlineEnvironment>
    {
//        std::cout << "solve_overload? hash: " << make_parameter_hash( args_env_ids ) << std::endl;

        auto const it = overloads_.find( make_parameter_hash( args_env_ids ) );

        return it != overloads_.cend() ? it->second : nullptr;
    }

    // delegate lookup
    auto lookup( literal::const_single_identifier_value_base_ptr const& name )
        -> env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->lookup( name ); }
    auto lookup( literal::const_single_identifier_value_base_ptr const& name ) const
        -> const_env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->lookup( name ); }
    auto find_on_env( literal::const_single_identifier_value_base_ptr const& name )
        -> env_pointer RILL_CXX11_OVERRIDE { return get_parent_env()->find_on_env( name ); }
    auto find_on_env( literal::const_single_identifier_value_base_ptr const& name ) const
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
/*        for( auto const& ins : instanced_env_ ) {
            os << indent
               << "symbol: " << ins.first
               << " / id: " << ins.second->get_id()
               << " / symbol kind: " << static_cast<int>( ins.second->get_symbol_kind() ) << std::endl;
        }*/
        return os;
    }
private:

    // todo map
    std::shared_ptr<InlineEnvironment> p_;
    std::unordered_map<parameter_hash_t, std::shared_ptr<InlineEnvironment>> overloads_;
    //const_environment_ptr ppp_;
};


/*
class has_parameter_environment RILL_CXX11_FINAL
    : public environment
{
public:
    has_parameter_environment( env_weak_pointer const& parent, function_definition_statement_base_ptr const& sp )
        : parent_( parent )
        , sp_( sp )
    {}

public:
    auto lookup_env( literal::identifier_value_ptr const& name ) const
        -> env_const_pointer
    { return nullptr; };

    auto get_stmt() const RILL_CXX11_OVERRIDE
        -> statement_ptr
    { return sp_; }

public:
    env_weak_pointer parent_;
    function_definition_statement_base_ptr sp_;
};*/








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
/*        for( auto const& ins : instanced_env_ ) {
            os << indent
               << "symbol: " << ins.first
               << " / id: " << ins.second->get_id()
               << " / symbol kind: " << static_cast<int>( ins.second->get_symbol_kind() ) << std::endl;
        }*/
        return os;
    }

private:

    //statement_list sp_;
    //symbol_kind kind_;
};

/*
//
class has_parameter_environment RILL_CXX11_FINAL
    : public environment
{
public:
    has_parameter_environment( env_weak_pointer const& parent, function_definition_statement_base_ptr const& sp )
        : parent_( parent )
        , sp_( sp )
    {}

public:
    auto lookup_env( literal::identifier_value_ptr const& name ) const
        -> env_const_pointer
    { return nullptr; };

    auto get_stmt() const RILL_CXX11_OVERRIDE
        -> statement_ptr
    { return sp_; }

public:
    env_weak_pointer parent_;
    function_definition_statement_base_ptr sp_;
};

*/






/*





class environment
    : public std::enable_shared_from_this<environment>
{
    typedef environment                         self_type;
    typedef std::weak_ptr<self_type>            self_weak_pointer;
    typedef std::shared_ptr<self_type>          self_pointer;
    typedef std::shared_ptr<self_type const>    self_const_pointer;
    typedef std::string                         symbol_type;

public:
    environment();
    environment( self_weak_pointer const& parent, statement_ptr const& s );
    virtual ~environment();

public:
    //
    auto add_function(
        function_definition_statement_base_ptr const& sp
        )
        -> self_pointer;

    // 
    auto add_class(
        native_class_definition_statement const& sp
        )
        -> self_pointer
    {
        return nullptr;
    }



    auto lookup_env( std::string const& name ) const
        -> self_const_pointer;

    auto get_stmt() const
        -> statement_ptr;

    auto is_exist( symbol_type const& ) const
        -> boost::optional<self_pointer>;

private:
    bool is_root() const;
    bool has_parent() const;

private:
    symbol_types symbol_type_;
    statement_ptr syntax_tree_;

    self_weak_pointer parent_;
    std::unordered_map<symbol_type, self_pointer> simple_env_;
    std::unordered_map<symbol_type, self_pointer> template_env_;
};
typedef std::shared_ptr<environment> environment_ptr;
typedef std::shared_ptr<environment const> const_environment_ptr;




class symbol_table
{
public:
};



class simple_identifier_table
{
};


class has_arguments_identifier_table
{
public:
    auto add_overload( environment_ptr const& parent, parameter_list const& parameter, function_definition_statement_base_ptr const& sp )
        -> const_environment_ptr
    {
        ppp_ = std::make_shared<environment>( parent, sp );
        return ppp_;
    }

    auto lookup( environment_ptr const& parent, parameter_list const& parameter ) const
        -> const_environment_ptr
    {
        if ( parameter.size() == 2 ) {
            return ppp_;
            //if ( parameter[0].type->type()-> )
        }

        return nullptr;
    }

private:
    const_environment_ptr ppp_;
};

class has_parameter_environment
{
public:

};


*/
