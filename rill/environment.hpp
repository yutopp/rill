#pragma once

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <unordered_map>

#include "structs.hpp"

typedef unsigned int    symbol_types_mask_t;
enum struct symbol_types : symbol_types_mask_t
{
    root_c = ( 1u << 0 )
};

class environment
    : public boost::enable_shared_from_this<environment>
{
    typedef environment                         self_type;
    typedef boost::weak_ptr<self_type>            self_weak_pointer;
    typedef boost::shared_ptr<self_type>          self_pointer;
    typedef boost::shared_ptr<self_type const>    self_const_pointer;
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
