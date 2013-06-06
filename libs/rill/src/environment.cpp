#include <rill/environment.hpp>
#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>

    environment::environment()
        : symbol_type_( symbol_types::root_c )
    {}

    environment::environment( self_weak_pointer const& parent, statement_ptr const& s )
        : symbol_type_( symbol_types::root_c )
        , syntax_tree_( s )
        , parent_( parent )
    {}

    environment::~environment()
    {}


    auto environment::add_function( statement_ptr const& sp )
        -> environment::self_pointer
    {
        symbol_type const symbol = "+";

        if ( nodes_.find( symbol ) != nodes_.cend() ) {
            // throw
            exit( -1 );
        }

        return nodes_[symbol] = std::make_shared<self_type>( shared_from_this(), sp );
    }

    auto environment::lookup_env( std::string const& name ) const
        -> environment::self_const_pointer
    {
        symbol_type const symbol = name;

        auto const& it = nodes_.find( symbol );
        if ( it != nodes_.cend() ) {
            return it->second;

        } else {
            if ( has_parent() ) {
                return parent_.lock()->lookup_env( name );
            } else {
                return nullptr;
            }
        }
    }

    auto environment::get_stmt() const
        -> statement_ptr
    {
        return syntax_tree_;
    }


    bool environment::is_root() const
    {
        return ( static_cast<symbol_types_mask_t>( symbol_type_ ) & static_cast<symbol_types_mask_t>( symbol_types::root_c ) ) != 0;
    }

    bool environment::has_parent() const
    {
        return is_root() && !parent_.expired();
    }