#include <rill/environment.hpp>
#include <rill/value.hpp>
#include <rill/expression.hpp>
#include <rill/statement.hpp>


/*
    single_identifier_environment_base::environment()
        : symbol_type_( symbol_types::root_c )
    {}

    environment::environment( self_weak_pointer const& parent, statement_ptr const& s )
        : symbol_type_( symbol_types::root_c )
        , syntax_tree_( s )
        , parent_( parent )
    {}

    environment::~environment()
    {}

*/

    auto single_identifier_environment_base::add_function(
        function_definition_statement_base_ptr const& sp
        ) -> env_pointer
    {
        auto const name = sp->get_identifier()->get_last_symbol()->get_native_symbol_string();

        /*
        if ( is_exist( symbol_name ) ) {
            // throw
            exit( -1 );
        }*/

        auto const p = std::make_shared<has_parameter_environment>( shared_from_this(), sp );
        //p->add_overload( shared_from_this(), sp->get_parameter_list(), sp );
        //sp->get_parameter_list();

        instanced_env_[name] = p;

        return instanced_env_[name];
    }


    auto single_identifier_environment_base::add_class( class_definition_statement_ptr const& sp )
        -> env_pointer
    {
        auto const& name = sp->get_symbol_name()->get_native_symbol_string();

        instanced_env_[name] = std::make_shared<class_identifier_environment>( shared_from_this(), sp );

        return instanced_env_[name];
    }




    


    auto single_identifier_environment_base::lookup_env( literal::identifier_value_ptr const& identifier ) const
        -> env_const_pointer
    {
        auto const& name = identifier->get_last_symbol()->get_native_symbol_string(); // TODO: change to do namespace search.

        if ( auto const env = is_exist_in_instanced( name ) ) {
            return *env;

        }/* else {
            if ( has_parent() ) {
                return parent_.lock()->lookup_env( name );
            } else {
                return nullptr;
            }
        }*/

        exit( -1 );
        return nullptr;
    }

/*
    auto environment::get_stmt() const
        -> statement_ptr
    {
        return syntax_tree_;
    }


 auto environment::is_exist( symbol_type const& symbol_name ) const
        -> boost::optional<environment::self_pointer>
 {
     auto const& it = nodes_.find( symbol_name );
     if ( it != nodes_.cend() )
         return it->second;

     return boost::none;
 }





    bool environment::is_root() const
    {
        return ( static_cast<symbol_types_mask_t>( symbol_type_ ) & static_cast<symbol_types_mask_t>( symbol_types::root_c ) ) != 0;
    }

    bool environment::has_parent() const
    {
        return is_root() && !parent_.expired();
    }
    */