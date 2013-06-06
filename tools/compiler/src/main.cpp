#include <iostream>

#include <rill/parser.hpp>

#include <memory>


#include <rill/environment.hpp>


struct compile_time_runtime
{
};


template<typename T>
void run( const_environment_ptr const& env, T const& prog )
{
    for( auto const& stmt : prog ) {
        stmt->eval( env );
    }
}




int main()
{
    auto const root_env = std::make_shared<environment>();

    {
        auto add_int_int = std::make_shared<native_function_definition_statement>(
            "+"
            , []( std::vector<value_ptr> const& args ) -> value_ptr {
                std::cout << args.size() << std::endl;
                return std::make_shared<literal::int32_value>(
                          std::dynamic_pointer_cast<literal::int32_value>( args[0] )->get_value()
                          + std::dynamic_pointer_cast<literal::int32_value>( args[1] )->get_value()
                          );
              }
            );

        root_env->add_function( add_int_int );
        //auto const int_class_env = root_env->add_class( "int" );

        //int_class_env->add_method( "+", "int", "int", "int" );
    }

    auto const v = parse( "1*2 ;");

    // first path(make simple symbol table)
    for( auto const& stmt : v.product ) {
        stmt->setup_environment( root_env );
    }

    // second path(check symbol and do instantiation)
    for( auto const& stmt : v.product ) {
        //stmt->instantiation( root_env );
        //stmt->semantic_analysis( root_env );
    }


    // last
    // in this run in interpreter
    run( root_env, v.product );



    {char c; std::cin >> c;}
}