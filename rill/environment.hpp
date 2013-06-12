#pragma once



#include <memory>
#include <unordered_map>

#include <boost/optional.hpp>

#include "config/macros.hpp"

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


template<typename>
struct kind_classifier;


template<>
struct kind_classifier<function_definition_statement_base_ptr>
{
    static const symbol_kind value = symbol_kind::function_k;
};




class environment
    : public std::enable_shared_from_this<environment>
{
public:
    typedef environment                         env_type;

    typedef std::shared_ptr<env_type>           env_pointer;
    typedef std::shared_ptr<env_type const>     env_const_pointer;
    typedef std::weak_ptr<env_type>             env_weak_pointer;

    typedef native_string_t                     native_string_type;

public:
    virtual auto add_class( class_definition_statement_ptr const& ) -> env_pointer { return nullptr; }
    virtual auto add_function( function_definition_statement_base_ptr const& ) -> env_pointer { return nullptr; }

    virtual auto lookup_env( literal::identifier_value_ptr const& name ) const
        -> env_const_pointer =0;

    virtual auto get_stmt() const
        -> statement_ptr = 0;

public:
    virtual bool is_root() const { return false; }

private:
};
//typedef environment::self_pointer           environment_ptr;
//typedef environment::const_self_pointer     const_environment_ptr;
//typedef environment::weak_self_pointer      weak_environment_ptr;





class template_environment RILL_CXX11_FINAL
    : public environment
{
public:
    template_environment( env_weak_pointer const& parent, void* )
        : parent_( parent )
    {}

public:
    auto lookup_env( literal::identifier_value_ptr const& name ) const RILL_CXX11_OVERRIDE
        -> env_const_pointer
    { return nullptr; };

    auto get_stmt() const RILL_CXX11_OVERRIDE
        -> statement_ptr
    { return nullptr; }

private:
    env_weak_pointer parent_;

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
    virtual ~single_identifier_environment_base() {};

public:
    virtual auto add_class( class_definition_statement_ptr const& ) -> env_pointer;

    virtual auto add_function( function_definition_statement_base_ptr const& ) -> env_pointer;




    auto lookup_env( literal::identifier_value_ptr const& name ) const
        -> env_const_pointer;
    
    virtual auto get_stmt() const
        -> statement_ptr
    { return nullptr; }

    
    auto is_exist_in_instanced( native_string_type const& name ) const
        -> boost::optional<env_pointer>
    {
        auto const& it = instanced_env_.find( name );
        if ( it != instanced_env_.cend() )
            return it->second;

        return boost::none;
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
    root_environment() {}

public:
    bool is_root() const RILL_CXX11_OVERRIDE
    {
        return true;
    }

private:

};





//
class class_identifier_environment RILL_CXX11_FINAL
    : public single_identifier_environment_base
{
public:
    class_identifier_environment( env_weak_pointer const& parent, class_definition_statement_ptr const& sp )
        : parent_( parent )
        , sp_( sp )
        //, kind_( kind )
    {}

public:
    auto get_stmt() const RILL_CXX11_OVERRIDE
        -> statement_ptr
    { return sp_; };

private:
    env_weak_pointer parent_;

    class_definition_statement_ptr sp_;
    //symbol_kind kind_;
};


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
