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

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>


void sighandler( int signum )
{
    ::signal( signum, SIG_DFL );

    std::cerr << "SIGNAL: " << signum << std::endl;
    rill::debug::dump_backtrace();
    exit( -1 );
}


struct settings
{
    std::string link_runtime_lib_path;
    std::string import_runtime_lib_dir;

    std::vector<std::string> sources;

    std::string output_name;
};


int build( settings const& st )
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
    if ( st.sources.size() > 1 ) {
        std::cerr << "sorry, only one input is accepted." << std::endl;
        return -1;
    }
    std::string const& f = st.sources[0];

    auto const cwd = fs::current_path();
    auto source_fullpath = cwd/f;
    std::cout << source_fullpath << std::endl;


    // syntax analysis
    auto const module
        = rill::syntax_analysis::parse( source_fullpath );
    if ( module == nullptr ) {
        std::cerr << "Failed at syntax analysis." << std::endl;
        return -10;
    }

    rill_dregion {
        // debug
        std::cout
            << "Top statements size: " << module->program->statements_.size() << std::endl;

        rill::debug::print_ast( module );
    }


    // semantic analysis
    std::cout << " ========================== " << std::endl;
    std::cout << " = Semantic Analysis ====== " << std::endl;

    auto sema_options = rill::semantic_analysis::analyzer_options{};
    sema_options.system_import_path.push_back( st.import_runtime_lib_dir );

    auto const& report
        = rill::semantic_analysis::analyse_and_complement(
            g_env, module, intrinsic_function_action, std::move( sema_options )
            );
    if ( report->is_errored() ) {
        std::cerr << "Failed at semantic analysis." << std::endl;
        return -20;
    }


    // generate llvm ir
    std::cout << " ========================== " << std::endl;
    std::cout << " = LLVM =================== " << std::endl;
    auto const& code_context
        = rill::code_generator::generate_llvm_ir(
            g_env, module, intrinsic_function_action
            );


    std::cout << " ========================== " << std::endl;
    std::cout << " = Bin =================== " << std::endl;
    auto const binary_gen
        = rill::code_generator::binary_generator_from_llvm_ir( code_context );
    // FIXME
    binary_gen.test( st.output_name, st.link_runtime_lib_path );

    return 0;
}


int main( int argc, char* argv[] )
{
    ::signal( SIGSEGV, sighandler );
    ::signal( SIGABRT, sighandler );

    namespace po = boost::program_options;

    // Generic options
    po::options_description generic( "Generic options" );
    generic.add_options()
        ( "version,v", "print version string" )
        ( "help", "produce help message" )
        ;

    //
    po::options_description config( "Configuration" );
    config.add_options()
        ( "rill-link-rt-lib-path",
          po::value<std::string>()->default_value( RILL_LINK_RUNTIME_LIB_PATH ),
          "rill runtime library path" )

        ( "rill-import-rt-lib-dir",
          po::value<std::string>()->default_value( RILL_IMPORT_RUNTIME_LIB_DIR ),
          "rill runtime library dir" )

        ( "output,o",
          po::value<std::string>()->default_value( "a.out" ),
          "name of output file" )

        ( "rill-rt-lib-path",
          po::value<std::string>(),
          "[obsolete option] rill runtime library path" )
        ;


    po::options_description hidden( "Hidden options" );
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

    po::options_description visible( "Allowed options" );
    visible
        .add( generic )
        .add( config );


    po::positional_options_description p;
    p.add("input-files", -1);


    try {
        po::variables_map vm;
        po::store(
            po::command_line_parser( argc, argv )
                .options( cmdline_options )
                .positional( p )
                .run(),
            vm
            );
        po::notify( vm );

        settings st;

        // if "help" option was passed, show options and exit
        if ( vm.count( "help" ) ) {
            std::cout << visible << std::endl;
            return 0;
        }

        // version
        if ( vm.count( "version" ) ) {
            std::cout << "rill compiler version "
                      << RILL_VERSION_MAJOR << "." << RILL_VERSION_MINOR << "." << RILL_VERSION_PATCHLEVEL << std::endl
                      << std::endl
                      << "RILL_LINK_RUNTIME_LIB_PATH : " << RILL_LINK_RUNTIME_LIB_PATH << std::endl
                      << "RILL_IMPORT_RUNTIME_LIB_DIR: " << RILL_IMPORT_RUNTIME_LIB_DIR << std::endl;
            return 0;
        }

        // libraries
        if ( vm.count( "rill-link-rt-lib-path" ) ) {
            st.link_runtime_lib_path = vm["rill-link-rt-lib-path"].as<std::string>();
        }
        if ( vm.count( "rill-import-rt-lib-dir" ) ) {
            st.import_runtime_lib_dir = vm["rill-import-rt-lib-dir"].as<std::string>();
        }

        if ( vm.count( "rill-rt-lib-path" ) ) {
            std::cerr << "the option 'rill-rt-lib-path' is obsolete." << std::endl
                      << "please use 'rill-link-rt-lib-path' instead." << std::endl;

            st.link_runtime_lib_path = vm["rill-rt-lib-path"].as<std::string>();
        }

        // sources
        if ( vm.count( "input-files" ) ) {
            for( auto const& input_file : vm["input-files"].as<std::vector<std::string>>() ) {
                std::cout << "input file: " << input_file << std::endl;
            }
            st.sources = vm["input-files"].as<std::vector<std::string>>();

        } else {
            std::cerr << "no input file" << std::endl;
            return -1;
        }

        //
        if ( vm.count( "output" ) ) {
            st.output_name = vm["output"].as<std::string>();
        }

        //
        return build( st );

    } catch( std::exception const& e ) {
        std::cerr << "Exception: " << std::endl
                  << e.what() << std::endl;
        return -1;
    }
}
