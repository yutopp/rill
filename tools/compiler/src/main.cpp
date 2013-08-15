//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>

#include <rill/syntax_analysis/make_syntax_tree.hpp>

#include <fstream>
#include <iterator>
#include <memory>


#include <rill/environment.hpp>







#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/interpreter/interpreter.hpp>

void sample()
{
    auto const root_env = std::make_shared<root_environment>();

    // operator +
    auto const operator_add
        = rill::ast::intrinsic::make_binary_operator_identifier( "+" );
    root_env->pre_construct( kind::function_k, operator_add );

    // operator *
    auto const operator_multiply
        = rill::ast::intrinsic::make_binary_operator_identifier( "*" );
    root_env->pre_construct( kind::function_k, operator_multiply );


    {
        // add int class definitions and operators

        auto const int_type
            = rill::ast::intrinsic::make_single_identifier( "int" );

        root_env->pre_construct( kind::class_k, int_type );
        root_env->construct( kind::class_k, int_type );


        /*
        auto const class_definition
            = make_native_class( int_type );
            */

        //
        //root_env->add_class( class_definition );

        {
            //
            parameter_list parameters;
            parameters.push_back( make_parameter_pair( make_identifier( int_type ) ) );
            parameters.push_back( make_parameter_pair( make_identifier( int_type ) ) );

            
            statement_list sl;
            sl.push_back(
                std::make_shared<return_statement>(
                    std::make_shared<embedded_function_call_expression>(
                        []( std::vector<const_value_ptr> const& args ) -> intrinsic::value_base_ptr {
                            std::cout << args.size() << std::endl;

                            return std::make_shared<intrinsic::int32_value>(
                                    std::dynamic_pointer_cast<intrinsic::int32_value const>( args[0] )->get_value()
                                    + std::dynamic_pointer_cast<intrinsic::int32_value const>( args[1] )->get_value()
                                    );
                        }
                        )
                    )
                );
            
            /*auto add_int_int = std::make_shared<native_function_definition_statement>(
                bin_op_function_name,
                parameters,
                int_type,
                []( std::vector<value_ptr> const& args ) -> value_ptr {
                    //std::cout << args.size() << std::endl;
                    return std::make_shared<intrinsic::int32_value>(
                              std::dynamic_pointer_cast<intrinsic::int32_value>( args[0] )->get_value()
                              + std::dynamic_pointer_cast<intrinsic::int32_value>( args[1] )->get_value()
                              );
                  }
                );*/

            // def +( :int, :int ): int => native
            root_env->construct( kind::function_k, operator_add, parameters, make_identifier( int_type ), sl );
        }

        {
            //
            parameter_list parameters;
            parameters.push_back( make_parameter_pair( make_identifier( int_type ) ) );
            parameters.push_back( make_parameter_pair( make_identifier( int_type ) ) );

            
            statement_list sl;
            sl.push_back(
                std::make_shared<return_statement>(
                    std::make_shared<embedded_function_call_expression>(
                        []( std::vector<const_value_ptr> const& args ) -> intrinsic::value_base_ptr {
                            std::cout << args.size() << std::endl;

                            return std::make_shared<intrinsic::int32_value>(
                                std::dynamic_pointer_cast<intrinsic::int32_value const>( args[0] )->get_value()
                                * std::dynamic_pointer_cast<intrinsic::int32_value const>( args[1] )->get_value()
                                );
                        }
                        )
                    )
                );
            
            /*auto add_int_int = std::make_shared<native_function_definition_statement>(
                bin_op_function_name,
                parameters,
                int_type,
                []( std::vector<value_ptr> const& args ) -> value_ptr {
                    //std::cout << args.size() << std::endl;
                    return std::make_shared<intrinsic::int32_value>(
                              std::dynamic_pointer_cast<intrinsic::int32_value>( args[0] )->get_value()
                              + std::dynamic_pointer_cast<intrinsic::int32_value>( args[1] )->get_value()
                              );
                  }
                );*/

            root_env->construct( kind::function_k, operator_multiply, parameters, make_identifier( int_type ), sl );
        }
        /*
        {
            auto const bin_op_function_name
                = intrinsic::make_binary_operator_identifier( intrinsic::make_symbol( "*" ) );

            //
            auto const parameters
                = make_parameter_list(
                        make_parameter_pair( int_type )
                        );


            auto add_int_int = std::make_shared<native_function_definition_statement>(
                bin_op_function_name,
                parameters,
                int_type,
                []( std::vector<value_ptr> const& args ) -> value_ptr {
                    //std::cout << args.size() << std::endl;
                    return std::make_shared<intrinsic::int32_value>(
                              std::dynamic_pointer_cast<intrinsic::int32_value>( args[0] )->get_value()
                              * std::dynamic_pointer_cast<intrinsic::int32_value>( args[1] )->get_value()
                              );
                  }
                );

            root_env->add_function( add_int_int );
        }*/
    }


    //
    // syntax analysis
    //

    // first(lexical & syntax)
    std::ifstream ifs( "input.rill" );
    if ( !ifs ) {
        std::cerr << "input.rill was not found..." << std::endl;
        exit( -100 );
    }
    std::istreambuf_iterator<char> const begin = ifs, end;
    native_string_t const input_source_code( begin, end );
    std::cout
        << "inputs are:" << std::endl
        << input_source_code << std::endl;

    //
    auto const syntax_tree = rill::syntax_analysis::make_syntax_tree( input_source_code );

    // debug
    std::cout
        << "Top statements size: " << syntax_tree->statements_.size() << std::endl;


    //
    // semantic analysis
    //

    // construct environment(symbol table)
    // following steps are processed recursive
    // first(1st pass. prove identifier)
    //   list all of identifiers
    // second(2nd pass. )
    //   check identifiers type and template instantiation
    std::cout << " = Semantic Analysis ====== " << std::endl;
    rill::semantic_analysis::analyse( root_env, syntax_tree );

    std::cout << " ========================== " << std::endl;


    // compile or interpret
    // last( debug )
    //rill::interpreter::run( root_env, v.statements_ );




    // 
    {
        std::cout << "======================================" << std::endl;
        auto env = root_env;
        std::string in;
        while( std::cin >> in ) {
            std::cout << "!e => finish identifier manager." << std::endl;
            if ( in == "!e" ) {
                std::cout << "see you" << std::endl;
                break;
            }
        }
    }

}



int main()
{
    sample();
}
