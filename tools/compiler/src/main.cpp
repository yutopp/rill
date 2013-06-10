#include <iostream>

#include <rill/make_syntax_tree.hpp>

#include <memory>


#include <rill/environment.hpp>


struct compile_time_runtime
{
};

/*
template<typename T>
void run( const_environment_ptr const& env, T const& prog )
{
    for( auto const& stmt : prog ) {
        stmt->eval( env );
    }
}*/








#include <rill/list_identifier_pass.hpp>
#include <rill/instantiation_and_semantic_analysis_pass.hpp>
#include <rill/interpret_pass.hpp>

template<typename Node, typename EnvironmentPtr>
auto list_identifier( list_identifier_pass const& pass, Node const& node, EnvironmentPtr const& env )
    -> decltype( node.dispatch( pass, env ) )
{
    return node.dispatch( pass, env );
}


template<typename T>
void make_environment( environment_ptr const& env, T const& statements )
{
    // program has an ownership
    list_identifier_pass lip;

    for( auto const& s : statements )
        list_identifier( lip, *s, env );
}




template<typename Node, typename EnvironmentPtr>
auto instantiation_and_semantic_analysis(
    instantiation_and_semantic_analysis_pass const& pass,
    Node const& node,
    EnvironmentPtr const& env
    )
    -> decltype( node.dispatch( pass, env ) )
{
    return node.dispatch( pass, env );
}


template<typename T>
void check_semantic_and_instantiation( environment_ptr const& env, T const& statements )
{
    instantiation_and_semantic_analysis_pass iasap;
    
    for( auto const& s : statements )
        instantiation_and_semantic_analysis( iasap, *s, env );
}



template<typename Tag, typename Node, typename EnvironmentPtr>
auto interpret(
    interpret_pass<Tag> const& pass,
    Node const& node,
    EnvironmentPtr const& env
    )
    -> decltype( node.dispatch( pass, env ) )
{
    return node.dispatch( pass, env );
}


template<typename Tag, typename T>
void interpret_program( environment_ptr const& env, T const& statements )
{
    interpret_pass<Tag> iasap;
    
    for( auto const& s : statements )
        interpret( iasap, *s, env );
}






int main()
{
    auto const root_env = std::make_shared<root_environment>();

    {
        // add int class definitions and operators

        auto const int_type
            = literal::make_simple_identifier( "int" );

        auto const class_definition
            = make_native_class( int_type );

        //
        root_env->add_class( class_definition );


        auto const bin_op_function_name
            = literal::make_binary_operator_identifier( literal::make_symbol( "*" ) );

        //
        auto const parameters
            = make_parameter_list(
                    make_parameter_pair( int_type )
                    );


        auto add_int_int = std::make_shared<native_function_definition_statement>(
            bin_op_function_name,
            int_type,
            parameters,
            []( std::vector<value_ptr> const& args ) -> value_ptr {
                std::cout << args.size() << std::endl;
                return std::make_shared<literal::int32_value>(
                          std::dynamic_pointer_cast<literal::int32_value>( args[0] )->get_value()
                          * std::dynamic_pointer_cast<literal::int32_value>( args[1] )->get_value()
                          );
              }
            );

        root_env->add_function( add_int_int );
        //auto const int_class_env = root_env->add_class( "int" );

        //int_class_env->add_method( "+", "int", "int", "int" );
    }

    // first(lexical & syntax)
    auto const v = make_syntax_tree( "4 *		2 ;");

    // second(1st pass. prove identifier)
    make_environment( root_env, v.product );


    // second(2nd pass. )
    check_semantic_and_instantiation( root_env, v.product );
    for( auto const& stmt : v.product ) {
        //stmt->setup_environment( root_env );
    }

    // second path(check symbol and do instantiation)
    for( auto const& stmt : v.product ) {

        //stmt->instantiation( root_env );
        //stmt->semantic_analysis( root_env );
    }


    // last( debug )
    interpret_program<runtime_interpret_tag>( root_env, v.product );


    {char c; std::cin >> c;}
}