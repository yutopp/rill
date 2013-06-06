#pragma once

#include <memory>
#include <unordered_map>

#include "statement.hpp"

typedef unsigned int    symbol_types_mask_t;
enum struct symbol_types : symbol_types_mask_t
{
    root_c = ( 1u << 0 )
};

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
    auto add_function( statement_ptr const& sp )
        -> self_pointer;

    auto lookup_env( std::string const& name ) const
        -> self_const_pointer;

    auto get_stmt() const
        -> statement_ptr;

private:
    bool is_root() const;

    bool has_parent() const;

private:
    symbol_types symbol_type_;
    statement_ptr syntax_tree_;

    self_weak_pointer parent_;
    std::unordered_map<symbol_type, self_pointer> nodes_;
};
