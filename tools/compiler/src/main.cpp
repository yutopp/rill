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

#include <boost/program_options.hpp>

#include <rill/rill.hpp>
#include <rill/environment/environment.hpp>
#include <rill/syntax_analysis/make_syntax_tree.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

#include <rill/code_generator/code_generator.hpp>


void sample( boost::program_options::variables_map const& vm )
{
    // create default rill world
    auto const& t = rill::create_world<>();
    auto const root_env = std::get<0>( t );
    auto const intrinsic_function_action = std::get<1>( t );


    //
    // syntax analysis
    //

    std::string const f = vm["input-files"].as<std::vector<std::string>>()[0];

    // first(lexical & syntax)
    std::ifstream ifs( f );
    if ( !ifs ) {
        std::cerr << f << " was not found..." << std::endl;
        exit( -100 );
    }
    std::istreambuf_iterator<char> const begin = ifs, end;
    rill::ast::native_string_t const input_source_code( begin, end );
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

//    rill::interpreter::run( root_env, intrinsic_function_action, parse_tree );

    //
    std::cout << " = LLVM =================== " << std::endl;
    rill::code_generator::generate_llvm_ir( root_env, intrinsic_function_action, parse_tree );



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

static const class A
{
public:
    A()
    {
        std::cout << "begin" << std::endl;
    }

    ~A()
    {
        std::cout << "end" << std::endl;
    }
} aa;


int main( int argc, char* argv[] )
{
    namespace po = boost::program_options;


    // Generic options
    po::options_description generic("Generic options");
    generic.add_options()
        ( "version,v", "print version string" )
        ( "help", "produce help message" )
        ;

    //
    po::options_description config("Configuration");
    config.add_options()
        ( "linker",
          po::value<std::string>()->default_value( "ld" ),
          "linker type(ld, link)")
        ( "rill-rt",
          po::value<std::string>()->default_value( "/usr/local/lib/librill-rt.a" ),
          "rill runtime library path")
        ( "output,o",
          po::value<std::string>()->default_value( "a.out" ),
          "gahaha!w")
        ;


    po::options_description hidden("Hidden options");
    hidden.add_options()
        ( "input-files", po::value<std::vector<std::string>>(), "input file" )
        ;


    po::options_description cmdline_options;
    cmdline_options
        .add( generic )
        .add( config )
        .add( hidden );

    po::options_description config_file_options;
    config_file_options
        .add( config )
        .add( hidden );

    po::options_description visible("Allowed options");
    visible
        .add( generic )
        .add( config );


    po::positional_options_description p;
    p.add("input-files", -1);

    po::variables_map vm;

    try {
        po::store( po::command_line_parser( argc, argv ).options( cmdline_options ).positional( p ).run(), vm );
        po::notify( vm );

        // if "help" option was passed, show options and exit
        if ( vm.count( "help" ) ) {
            std::cout << visible << std::endl;
            return 1;
        }

        //
        if ( vm.count( "input-files" ) ) {
            for( auto const& input_file : vm["input-files"].as<std::vector<std::string>>() ) {
                std::cout << "Input files are: "
                          << input_file << std::endl;
            }
        } else {
            std::cerr << "Please specify source code" << std::endl;
            return -1;
        }

    } catch( std::exception const& e ) {
        // 実際には引数に指定されなかった値を operator[] でアクセスしようとすると
        // 例外が帰ってきたりする
        std::cerr << e.what() << std::endl;
        return -1;
    }


    sample( vm );
}
