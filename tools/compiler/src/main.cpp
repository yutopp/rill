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
#include <boost/filesystem.hpp>

#include <rill/rill.hpp>
#include <rill/environment/environment.hpp>
#include <rill/syntax_analysis/parse.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>
#include <rill/code_generator/code_generator.hpp>

#include <rill/debug/debug.hpp>


void sample( boost::program_options::variables_map const& vm )
{
    namespace fs = boost::filesystem;

    auto const& host
        = std::make_shared<rill::behavior::default_system_info_setter<rill::behavior::x86_64_tag>>();

    auto const& target
        = std::make_shared<rill::behavior::default_system_info_setter<rill::behavior::x86_64_tag>>();

    // create default rill world
    auto const& t = rill::create_world<>( host, target );

    auto const g_env = std::get<0>( t );
    auto const intrinsic_function_action = std::get<1>( t );



    //
    // syntax analysis
    //

    std::string const f = vm["input-files"].as<std::vector<std::string>>()[0];

    auto const cwd = fs::current_path();
    auto source_fullpath = cwd/f;
    std::cout << source_fullpath << std::endl;

    //
    auto const module
        = rill::syntax_analysis::parse( source_fullpath );
    if ( module == nullptr ) {
        std::cerr << "Failed at syntax analysis." << std::endl;
        exit( -200 );
    }

    // debug
    debug_s {
        std::cout
            << "Top statements size: " << module->program->statements_.size() << std::endl;

        rill::debug::print_ast( module );
    }




    //
    // semantic analysis
    //

    // construct environment(symbol table)
    // following steps are processed recursive
    // first(1st pass. prove identifier)
    //   list all of identifiers
    // second(2nd pass. )
    //   check identifiers type and template instantiation
    std::cout << " ========================== " << std::endl;
    std::cout << " = Semantic Analysis ====== " << std::endl;

    auto sema_options = rill::semantic_analysis::analyzer_options{};
    sema_options.system_import_path.push_back( "/home/yutopp/rill/rill-rt/src" );

    auto const& report
        = rill::semantic_analysis::analyse_and_complement(
            g_env, module, intrinsic_function_action, std::move( sema_options )
            );
    if ( report->is_errored() ) {
        std::cerr << "Failed at semantic analysis." << std::endl;
        exit( -230 );
    }

    // compile or interpret
    // last( debug )
    std::cout << " ========================== " << std::endl;
    std::cout << " = LLVM =================== " << std::endl;
    auto const& code_context
        = rill::code_generator::generate_llvm_ir(
            g_env, module, intrinsic_function_action
            );


    // FIXME
    std::string const output_name
        = vm["output"].as<std::string>();
    std::string const runtime_lib_path
        = vm["rill-rt-lib-path"].as<std::string>();

    auto const binary_gen
        = rill::code_generator::binary_generator_from_llvm_ir( code_context );
    binary_gen.test( output_name, runtime_lib_path );

}

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

void sighandler( int signum )
{
    ::signal( signum, SIG_DFL );

    rill::debug::dump_backtrace();
    abort();
}


int main( int argc, char* argv[] )
{
    ::signal( SIGSEGV, sighandler );

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
        ( "rill-rt-lib-path",
          po::value<std::string>()->default_value( "/usr/local/lib/librill-rt.a" ),
          "rill runtime library path")
        ( "output,o",
          po::value<std::string>()->default_value( "a.out" ),
          "name of output file")
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
        std::cerr << e.what() << std::endl;
        return -1;
    }


    sample( vm );
}
