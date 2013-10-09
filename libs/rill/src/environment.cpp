//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream> // debug

#include <rill/environment.hpp>

#include <rill/ast/value.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/statement.hpp>


namespace rill
{
    // --
    // single_identifier_environment_base
    // --

    auto environment::mark_as(
        kind::function_tag,
        intrinsic::single_identifier_value_base_ptr const& name_identifier,
        ast::statement_ptr const& ast
        )
        -> decltype( static_cast<environment *>( nullptr )->incomplete_construct( kind::function_tag(), name_identifier ) )
    {
        // construct incomplete environment( parameter wrapper & function )
        auto const p = incomplete_construct( kind::function_tag(), name_identifier );
        auto const& has_param_env = p.first;
        auto const& created_function_env = p.second;

        std::cout << "Marked, %&%& " << has_param_env->get_id() << " : " << created_function_env->get_id() << std::endl;

        //
        root_shared_resource_->env_id_to_ast_map.add( has_param_env->get_id(), ast );           // 
        root_shared_resource_->env_id_to_ast_map.add( created_function_env->get_id(), ast );    // related environment of created_function_env is parent envitroment of it

        //
        root_shared_resource_->ast_to_env_id_map.add( ast, created_function_env->get_id() );

        return p;
    }

    // function constructor
    auto single_identifier_environment_base::incomplete_construct(
        kind::function_tag,
        ast::intrinsic::single_identifier_value_base_ptr const& name
        )
        -> std::pair<
                std::shared_ptr<has_parameter_environment<function_symbol_environment>>,
                function_symbol_environment_ptr
           >
    {
        auto const& symbol_name = name->get_inner_symbol()->to_native_string();

        // need parameter wrapper environment because function has parameter information
        auto const& parameter_env = [&]() {
            if ( !is_instanced( symbol_name ) ) {
                // make uncomplete env
                auto const& w_env = allocate_env<has_parameter_environment<function_symbol_environment>>( shared_from_this() );
                instanced_env_[symbol_name] = w_env;
            }
            auto const& env = instanced_env_[symbol_name];
            assert( env != nullptr );
            assert( env->get_symbol_kind() == kind::type_value::parameter_wrapper_e );
            assert( std::dynamic_pointer_cast<has_parameter_environment_base>( env )->get_inner_symbol_kind() == kind::type_value::function_e );

            return std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment>>( env );
        }();

        // allocate incomplete funciton environment
        auto const& incomplete_function_env = parameter_env->allocate_inner_env_as_incomplete();

        return std::make_pair( parameter_env, incomplete_function_env );
    }


    auto single_identifier_environment_base::construct(
            kind::function_tag,
            intrinsic::single_identifier_value_base_ptr const& name,
            function_env_generator_scope_type const& parameter_decl_initializer,
            class_symbol_environment_ptr const& return_type_env,
            ast::statement_ptr const& ast
            ) -> function_symbol_environment_ptr
    {
        auto const& p_i_pair = mark_as( kind::function_k, name, ast );
        auto const& parameter_env = p_i_pair.first;

        auto const& incomplete_function_env = p_i_pair.second;

        // complete parameter decl
        auto const& parameter_completed_function_env_pointer = parameter_decl_initializer( incomplete_function_env );
        
        // complete return type, name
        parameter_completed_function_env_pointer->complete( return_type_env, name->get_inner_symbol()->to_native_string() );

        //
        parameter_env->add_overload( parameter_completed_function_env_pointer );

        return parameter_completed_function_env_pointer;
    }





    // variable
    auto single_identifier_environment_base::construct(
        kind::variable_tag,
        intrinsic::single_identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable...
        const_class_symbol_environment_ptr const& type_env
        ) -> variable_symbol_environment_ptr
    {
        auto const& w_env = allocate_env<variable_symbol_environment>( shared_from_this() );

        native_string_t const& symbol_name
            = variable_name
            ? variable_name->get_inner_symbol()->to_native_string()
            : "__unnamed" + std::to_string( w_env->get_id() )
            ;

        // complete return type, name
        w_env->complete( type_env, symbol_name );

        std::cout << "instanced!: " << symbol_name << std::endl;


        instanced_env_[symbol_name] = w_env;
        return w_env;
    }



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
    auto const it = instanced_env_.find( name->get_inner_symbol()->to_native_string() );

    return ( it != instanced_env_.end() ) ? it->second : nullptr;
}

auto single_identifier_environment_base::find_on_env( intrinsic::const_single_identifier_value_base_ptr const& name ) const
    -> const_env_pointer
{
    auto const it = instanced_env_.find( name->get_inner_symbol()->to_native_string() );

    return ( it != instanced_env_.end() ) ? it->second : nullptr;
}


auto single_identifier_environment_base::lookup( intrinsic::const_single_identifier_value_base_ptr const& name )
    -> env_pointer
{
    if ( name->is_template() ) {
        // TODO: add template support
        assert( false );
        return nullptr;

    } else {
        auto const s = find_on_env( name );

        return s
                ? s
                : is_root()
                    ? nullptr   // Not found...
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




/*
auto single_identifier_environment_base::construct(
    kind::function_tag,
    intrinsic::single_identifier_value_base_ptr const& name,
    type_environment_ptr_list const& plist,
    intrinsic::identifier_value_ptr const& return_type,
    statement_list const& statements
    )
    -> env_pointer
{
    // TODO: add existance check

    auto const& env = instanced_env_[name->get_inner_symbol()->to_native_string()];

    if (  env->get_symbol_kind() != kind::type_value::parameter_wrapper_e
       || std::dynamic_pointer_cast<has_parameter_environment_base>( env )->get_inner_symbol_kind() != kind::type_value::function_e
       ) {
        //
        exit( -900 );
    }

    auto const& f_env = std::dynamic_pointer_cast<has_parameter_environment<function_symbol_environment>>( env );
    auto const& function = f_env->add_overload( plist, statements );

    return function;
}*/






// class(type)
auto single_identifier_environment_base::pre_construct(
    kind::class_tag,
    intrinsic::single_identifier_value_ptr const& name
    )
    -> env_pointer
{
    // make uncomplete env
    auto const& w_env = allocate_env<class_symbol_environment>( shared_from_this() );

    instanced_env_[name->get_inner_symbol()->to_native_string()] = w_env;
    return w_env;
}

auto single_identifier_environment_base::construct(
    kind::class_tag,
    intrinsic::single_identifier_value_base_ptr const& name
    )
    -> class_symbol_environment_ptr
{
    // TODO: add existance check

    auto const& env = instanced_env_[name->get_inner_symbol()->to_native_string()];

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








    auto function_symbol_environment::parameter_variable_construct(
        /* ,*/
        intrinsic::single_identifier_value_base_ptr const& variable_name,   // may be nullptr, if unnamed parameter variable
        const_class_symbol_environment_ptr const& type_env
        )
        -> variable_symbol_environment_ptr
    {
        // declare parameter variable
        auto const& var_env = construct( kind::variable_k, variable_name, type_env );
        parameter_decl_ids_.push_back( var_env->get_id() );
        parameter_type_ids_.push_back( type_env->get_id() );    // memo parameter variable types

        return var_env;
    }

} // namespace rill
