#pragma once

#include <vector>
#include <string>

#include "statement_fwd.hpp"

#include "value.hpp"
#include "expression.hpp"

#include "environment_fwd.hpp"
#include "tree_visitor_base.hpp"





// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// statements
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
struct statement
{
    virtual ~statement()
    {};

public:
    virtual void dispatch( tree_visitor_base const&, environment_ptr const& ) const =0;
};

//
#define ADAPT_STATEMENT_VISITOR( class_name ) \
    public: \
        virtual void dispatch( tree_visitor_base const& visitor, environment_ptr const& env ) const \
        { \
            visitor( *this, env ); \
        }







template<typename Target>
struct template_statement
    : statement
{
    ADAPT_STATEMENT_VISITOR( template_statement )
};




class expression_statement
    : public statement
{
    ADAPT_STATEMENT_VISITOR( expression_statement )

public:
    expression_statement( expression_ptr const& expr )
        : expression_( expr )
    {}

private:
    expression_ptr const expression_;
};





struct function_definition_statement_base
    : public statement
{
    ADAPT_STATEMENT_VISITOR( function_definition_statement_base )

public:
    function_definition_statement_base(
        literal::identifier_value_ptr const& symbol_name,
        literal::identifier_value_ptr const& return_type,
        parameter_list const& parameter_list
        )
        : symbol_name_( symbol_name )
        , return_type_( return_type )
        , parameter_list_( parameter_list )
    {}

public:
    auto get_symbol_name() const
        -> literal::identifier_value_ptr
    {
        return symbol_name_;
    }

    auto get_parameter_list() const
        -> parameter_list
    {
        return parameter_list_;
    }

private:
    literal::identifier_value_ptr symbol_name_;
    literal::identifier_value_ptr return_type_;
    parameter_list parameter_list_;
};











/*
class function_definition_statement
    : public function_definition_statement_base
{
public:
    function_definition_statement( expression_ptr const& expr );

public:
    void setup_environment( environment_ptr const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

    auto get_symbol_name() const
        -> literal::symbol_value_ptr
    {
        return nullptr;
    }

private:
    expression_ptr expression_;
};
typedef std::shared_ptr<function_definition_statement> function_definition_statement_ptr;



*/















#include <functional>
typedef std::function<value_ptr(std::vector<value_ptr> const&)> native_function_t;

struct native_function_definition_statement
    : public function_definition_statement_base
{
    ADAPT_STATEMENT_VISITOR( native_function_definition_statement )

public:
    native_function_definition_statement(
        literal::identifier_value_ptr const& symbol_name,
        literal::identifier_value_ptr const& return_type,
        parameter_list const& parameter_list,
        native_function_t const& callee
        )
        : function_definition_statement_base( symbol_name, return_type, parameter_list )
        , callee_( callee )
    {}

public:/*
    void setup_environment( environment_ptr const& ) const;

    void call( std::vector<value_ptr> const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;*/

private:
    native_function_t callee_;
};





struct class_definition_statement
    : public statement
{
    ADAPT_STATEMENT_VISITOR( class_definition_statement )

public:
    class_definition_statement( literal::identifier_value_ptr const& identifier )
        : identifier_( identifier )
    {}

    //virtual ~class_definition_statement {}

public:
    //void setup_environment( environment_ptr const& ) const {}

    auto get_identifier_name() const
        -> literal::identifier_value_ptr
    {
        return identifier_;
    }

    auto get_symbol_name() const
        -> literal::symbol_value_ptr
    {
        return identifier_->get_last_symbol();
    }

    //void eval( const_environment_ptr const& env ) const {}

private:
    literal::identifier_value_ptr identifier_;
};



// make native
inline auto make_native_class( literal::identifier_value_ptr const& class_name )
    -> class_definition_statement_ptr
{
    // TODO: insert assert that checks class_name depth.

    return std::make_shared<class_definition_statement>( class_name );
}








typedef std::vector<statement_ptr>  program;

typedef std::vector<statement_ptr>  statement_list;





#undef ADAPT_STATEMENT_VISITOR