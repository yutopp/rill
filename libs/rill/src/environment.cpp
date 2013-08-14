//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/environment.hpp>
#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>

std::ostream& operator<<( std::ostream& os, environment_ptr const& env )
{
    os << "DEBUG: environment" << std::endl;
    auto e = env;
    std::string indent = "^ ";

    if ( e ) {
        while( !e->is_root() ) {
            e->dump( os, indent );
            e = e->get_parent_env();
            indent += "  ";
        }
        return e->dump( os, indent );
    } else {
        os << indent << "nullptr." << std::endl;
        return os;
    }
}

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



//
auto single_identifier_environment_base::find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name )
    -> env_pointer
{
    auto const it = instanced_env_.find( name->get_base_symbol()->get_native_string() );

    return ( it != instanced_env_.end() ) ? it->second : nullptr;
}

auto single_identifier_environment_base::find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name ) const
    -> const_env_pointer
{
    auto const it = instanced_env_.find( name->get_base_symbol()->get_native_string() );

    return ( it != instanced_env_.end() ) ? it->second : nullptr;
}


auto single_identifier_environment_base::lookup( intrinsic::const_single_identifier_value_base_ptr const& name )
    -> env_pointer
{
    if ( name->is_template() ) {
        // TODO: add template support
        return nullptr;

    } else {
        auto const s = find_on_env( name );

        return s
                ? s
                : is_root()
                    ? nullptr
                    : get_parent_env()->lookup( name )
                    ;
    }
}

auto single_identifier_environment_base::lookup( intrinsic::const_single_identifier_value_base_ptr const& name ) const
    -> const_env_pointer
{
    if ( name->is_template() ) {
        // TODO: add template support
        return nullptr;

    } else {
        auto const s = find_on_env( name );

        return s
                ? s
                : is_root()
                    ? nullptr
                    : get_parent_env()->lookup( name )
                    ;
    }
}


// function
auto single_identifier_environment_base::pre_construct(
    kind::function_tag,
    intrinsic::single_identifier_value_base_ptr const& name
    )
    -> env_pointer
{
    // make uncomplete env
    auto const& w_env = allocate_env<has_parameter_environment<function_symbol_environment>>( shared_from_this() );

    instanced_env_[name->get_base_symbol()->get_native_string()] = w_env.pointer;
    return w_env.pointer;
}

auto single_identifier_environment_base::construct(
    kind::function_tag,
    intrinsic::single_identifier_value_base_ptr const& name,
    parameter_list const& plist,
    intrinsic::identifier_value_ptr const& return_type,
    statement_list const& statements
    )
    -> env_pointer
{
    // TODO: add existance check

    auto const& env = instanced_env_[name->get_base_symbol()->get_native_string()];

    if (  env->get_symbol_kind() != kind::type_value::parameter_wrapper_e
       || std::dynamic_pointer_cast<has_parameter_environment_base>( env )->get_inner_symbol_kind() != kind::type_value::function_e
       ) {
        exit( -900 );
    }

    auto const& f_env = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment>>( env );
    auto const& function = f_env->add_overload( plist, statements );

    return function;
}


// variable
auto single_identifier_environment_base::pre_construct(
    kind::variable_tag,
    intrinsic::single_identifier_value_base_ptr const& name
    ) -> env_pointer
{
    auto const& w_env = allocate_env<variable_symbol_environment>( shared_from_this() );

    instanced_env_[name->get_base_symbol()->get_native_string()] = w_env.pointer;
    return w_env.pointer;
}

auto single_identifier_environment_base::construct(
    kind::variable_tag,
    intrinsic::single_identifier_value_base_ptr const& name,
    environment_id_t const& value_type_env_id
    ) -> env_pointer
{
    auto const& w_env = allocate_env<variable_symbol_environment>( shared_from_this(), value_type_env_id );

    instanced_env_[name->get_base_symbol()->get_native_string()] = w_env.pointer;
    return w_env.pointer;
}


// class
auto single_identifier_environment_base::pre_construct(
    kind::class_tag,
    intrinsic::single_identifier_value_ptr const& name
    )
    -> env_pointer
{
    // make uncomplete env
    auto const& w_env = allocate_env<class_symbol_environment>( shared_from_this() );

    instanced_env_[name->get_base_symbol()->get_native_string()] = w_env.pointer;
    return w_env.pointer;
}

auto single_identifier_environment_base::construct(
    kind::class_tag,
    intrinsic::single_identifier_value_base_ptr const& name
    )
    -> env_pointer
{
    // TODO: add existance check

    auto const& env = instanced_env_[name->get_base_symbol()->get_native_string()];

    if ( env->get_symbol_kind() != kind::type_value::class_e ) {
        exit( -900 );
    }

    auto const& c_env = std::dynamic_pointer_cast<class_symbol_environment>( env );

    return c_env;
}
/*


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