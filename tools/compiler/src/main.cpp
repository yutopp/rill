//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>

#include <fstream>
#include <iterator>
#include <memory>


#include <rill/environment.hpp>
#include <rill/syntax_analysis/make_syntax_tree.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

#include <rill/interpreter/interpreter.hpp>
#include <rill/interpreter/runtime.hpp>
#include <rill/code_generator/code_generator.hpp>


void sample()
{




    //
    // prepareation for semantic analysis
    // it makes core.lang
    //
    auto const root_env = std::make_shared<rill::root_environment>();
    auto const embedded_function_action = std::make_shared<rill::embedded_function_holder>();

    // TODO: add core.lang namespace

    // operator +
    auto const operator_add
        = rill::ast::intrinsic::make_binary_operator_identifier( "+" );
//    root_env->pre_construct( rill::kind::function_k, operator_add );

    auto const operator_multiply
        = rill::ast::intrinsic::make_binary_operator_identifier( "*" );
// /   root_env->pre_construct( rill::kind::function_k, operator_multiply );
/*
    // operator *
    auto const operator_multiply
        = rill::ast::intrinsic::make_binary_operator_identifier( "*" );
    root_env->pre_construct( kind::function_k, operator_multiply );
    */


    {
        // add int class definitions and operators

        auto const int_type
            = rill::ast::intrinsic::make_single_identifier( "int" );

        root_env->pre_construct( rill::kind::class_k, int_type );
        auto const int_class_env_pointer = root_env->construct( rill::kind::class_k, int_type );



        {
            //
            // def +( :int, :int ): int => native
            //

            struct operator_add_action
                : rill::embedded_function_action_base
            {
                // for debug interpreter
                auto invoke( rill::processing_context::debug_interpreter_tag, rill::interpreter::context_ptr const& context ) const
                    -> rill::ast::intrinsic::value_base_ptr
                {
                    auto const& args = context->current_scope()->get_parameter_variable();
                    std::cout << "operator + ! :: args_num -> " << args.size() << std::endl;

                    std::cout << *args[0] << "+" << *args[1] << std::endl;

                    return std::make_shared<rill::ast::intrinsic::int32_value>(
                            std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[0] )->get_value()
                            + std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[1] )->get_value()
                            );
                }
            };
            auto const action_id = embedded_function_action->append<operator_add_action>();

            // function body
            rill::ast::statement_list sl;
            sl.push_back(
                std::make_shared<rill::ast::return_statement>(
                    std::make_shared<rill::ast::embedded_function_call_expression>( action_id )
                    )
                );
            auto ast = std::make_shared<rill::embedded_function_definition_statement>( sl );


            // function definition
            auto f = root_env->construct( rill::kind::function_k, operator_add, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :int, :int )
                std::cout << "add operator +!!" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int
                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int

                return fenv;
            }, int_class_env_pointer, ast );
        }

        {
            //
            // def *( :int, :int ): int => native
            //
            struct operator_add_action
                : rill::embedded_function_action_base
            {
                // for debug interpreter
                auto invoke( rill::processing_context::debug_interpreter_tag, rill::interpreter::context_ptr const& context ) const
                    -> rill::ast::intrinsic::value_base_ptr
                {
                    auto const& args = context->current_scope()->get_parameter_variable();
                    std::cout << "operator * ! :: args_num -> " << args.size() << std::endl;

                    return std::make_shared<rill::ast::intrinsic::int32_value>(
                            std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[0] )->get_value()
                            * std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[1] )->get_value()
                            );
                }
            };
            auto const action_id = embedded_function_action->append<operator_add_action>();

            // function body
            rill::ast::statement_list sl;
            sl.push_back(
                std::make_shared<rill::ast::return_statement>( std::make_shared<rill::ast::embedded_function_call_expression>( action_id ) )
                );
            auto ast = std::make_shared<rill::embedded_function_definition_statement>( sl );

            // function definition
            auto f = root_env->construct( rill::kind::function_k, operator_multiply, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :int, :int )
                std::cout << "add operator *!!" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int
                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int

                return fenv;
            }, int_class_env_pointer, ast );
        }
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
    rill::native_string_t const input_source_code( begin, end );
    std::cout
        << "inputs are:" << std::endl
        << input_source_code << std::endl;

    //
    auto const parse_tree = rill::syntax_analysis::make_syntax_tree( input_source_code );

    // debug
    std::cout
        << "Top statements size: " << parse_tree->statements_.size() << std::endl;


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

    rill::semantic_analysis::analyse_and_complement( root_env, parse_tree );

    


    // compile or interpret
    // last( debug )
    std::cout << " ========================== " << std::endl;

    rill::interpreter::run( root_env, embedded_function_action, parse_tree );

    //
    std::cout << " = LLVM =================== " << std::endl;
    rill::code_generator::generate_llvm_ir( root_env, embedded_function_action, parse_tree );



    // Not implemented...
    {
        std::cout << "======================================" << std::endl;
        auto env = root_env;
        std::string in;
        while( std::cin >> in ) {
            std::cout << "!e => finish identifier manager." << std::endl;
            if ( in == "!e" ) {
                std::cout << "see you!" << std::endl;
                break;
            }
        }
    }

}



int main()
{
    sample();
}
