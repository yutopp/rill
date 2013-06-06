#pragma once

#include <vector>
#include <string>
#include <memory>

#include "value.hpp"
#include "expression.hpp"

#include "environment_fwd.hpp"

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// statements
//
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
class statement
{
public:
    virtual ~statement();

public:
    virtual void setup_environment( environment_ptr const& ) const =0;
    virtual void eval( const_environment_ptr const& ) const =0;
};
typedef std::shared_ptr<statement> statement_ptr;



class expression_statement
    : public statement
{
public:
    expression_statement( expression_ptr const& expr );

public:
    void setup_environment( environment_ptr const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

private:
    expression_ptr expression_;
};
typedef std::shared_ptr<expression_statement> expression_statement_ptr;



class function_definition_statement
    : public statement
{
public:
    function_definition_statement( expression_ptr const& expr );

public:
    void setup_environment( environment_ptr const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

private:
    expression_ptr expression_;
};
typedef std::shared_ptr<function_definition_statement> function_definition_statement_ptr;



















#include <functional>
typedef std::function<value_ptr(std::vector<value_ptr> const&)> native_function_t;

class native_function_definition_statement
    : public statement
{
public:
    native_function_definition_statement( std::string const& name, native_function_t const& callee );

public:
    void setup_environment( environment_ptr const& ) const;

    void call( std::vector<value_ptr> const& ) const;

    void eval( const_environment_ptr const& env ) const;

    void instantiation( environment_ptr const& root_env ) const;
    void semantic_analysis( environment_ptr const& root_env ) const;

private:
    std::string name_;
    native_function_t callee_;
};
typedef std::shared_ptr<function_definition_statement> function_definition_statement_ptr;








typedef std::string symbol;
typedef std::vector<statement_ptr>  program;