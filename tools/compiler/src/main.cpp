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

#include <rill/environment.hpp>
#include <rill/syntax_analysis/make_syntax_tree.hpp>
#include <rill/semantic_analysis/semantic_analysis.hpp>

#include <rill/interpreter/interpreter.hpp>
#include <rill/interpreter/runtime.hpp>
#include <rill/code_generator/code_generator.hpp>


// TODO: remove these functions
template<typename Action, typename ActionHolder, typename RootEnv, typename Identifier, typename ParameterConstructor, typename Env>
void construct_predefined_function(
    ActionHolder& action_holder,
    RootEnv& root_env,
    Identifier const& function_name,
    ParameterConstructor const& tpc_func,
    Env const& return_type_env
    )
{
    // allocate the new action holder
    auto const action_id = action_holder->template append<Action>();

    // function body
    auto const& embedded_call_expr = std::make_shared<rill::ast::embedded_function_call_expression>( action_id );
    rill::ast::statement_list const sl = { std::make_shared<rill::ast::return_statement>( embedded_call_expr ) };
    auto ast = std::make_shared<rill::embedded_function_definition_statement>( sl );

    // function definition
    auto f = root_env->construct( rill::kind::function_k, function_name, tpc_func, return_type_env, ast );

    // memoize called function env
    f->connect_from_ast( embedded_call_expr );
}

// It defined a function that contains native machine code.
// machine code of function entry(brigde of parameters...) and function exit(cleanup) will be generated normally, but
// function body will be replaced this machine code.




void sample( int argc, char* argv[] )
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


    auto const operator_sub
        = rill::ast::intrinsic::make_binary_operator_identifier( "-" );
    auto const operator_divide
        = rill::ast::intrinsic::make_binary_operator_identifier( "/" );





    auto const print
        = rill::ast::intrinsic::make_single_identifier( "print" );
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


        auto const string_type
            = rill::ast::intrinsic::make_single_identifier( "string" );
        root_env->pre_construct( rill::kind::class_k, string_type );
        auto const string_class_env_pointer = root_env->construct( rill::kind::class_k, string_type );


        auto const void_type
            = rill::ast::intrinsic::make_single_identifier( "void" );
        root_env->pre_construct( rill::kind::class_k, void_type );
        auto const void_class_env_pointer = root_env->construct( rill::kind::class_k, void_type );




        {
            //
            // def +( :int, :int ): int => native
            //

            struct operator_add_action
                : rill::embedded_function_action_base
            {
                // for debug interpreter
                auto invoke(
                    rill::processing_context::debug_interpreter_tag,
                    rill::interpreter::context_ptr const& context
                    ) const
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

                //
                auto invoke(
                    rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                    std::shared_ptr<llvm::Module> const& module,
                    std::shared_ptr<llvm::IRBuilder<>> const& builder,
                    std::shared_ptr<rill::code_generator::llvm_ir_generator::env_id_llvm_table> const& llvm_table,
                    std::vector<rill::environment_id_t> const& parameter_variable_decl_env_ids
                    ) const
                    -> llvm::Value*
                {
                    return builder->CreateAdd( llvm_table->ref_value( parameter_variable_decl_env_ids[0] ), llvm_table->ref_value( parameter_variable_decl_env_ids[1] ) );
                }
            };

            construct_predefined_function<operator_add_action>( embedded_function_action, root_env, operator_add, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :int, :int )
                std::cout << "add operator +!!" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int
                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int

                return fenv;
            }, int_class_env_pointer );
        }

        {
            //
            // def *( :int, :int ): int => native
            //
            struct operator_multiply_action
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

                auto invoke(
                    rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                    std::shared_ptr<llvm::Module> const& module,
                    std::shared_ptr<llvm::IRBuilder<>> const& builder,
                    std::shared_ptr<rill::code_generator::llvm_ir_generator::env_id_llvm_table> const& llvm_table,
                    std::vector<rill::environment_id_t> const& parameter_variable_decl_env_ids
                    ) const
                    -> llvm::Value*
                {
                    return builder->CreateMul( llvm_table->ref_value( parameter_variable_decl_env_ids[0] ), llvm_table->ref_value( parameter_variable_decl_env_ids[1] ) );
                }
            };
            construct_predefined_function<operator_multiply_action>( embedded_function_action, root_env, operator_multiply, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :int, :int )
                std::cout << "add operator *!!" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int
                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int

                return fenv;
            }, int_class_env_pointer );
        }


        {
            //
            // def -( :int, :int ): int => native
            //

            struct operator_sub_action
                : rill::embedded_function_action_base
            {
                // for debug interpreter
                auto invoke( rill::processing_context::debug_interpreter_tag, rill::interpreter::context_ptr const& context ) const
                    -> rill::ast::intrinsic::value_base_ptr
                {
                    auto const& args = context->current_scope()->get_parameter_variable();
                    std::cout << "operator + ! :: args_num -> " << args.size() << std::endl;

                    std::cout << *args[0] << "-" << *args[1] << std::endl;

                    return std::make_shared<rill::ast::intrinsic::int32_value>(
                            std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[0] )->get_value()
                            - std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[1] )->get_value()
                            );
                }

                auto invoke(
                    rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                    std::shared_ptr<llvm::Module> const& module,
                    std::shared_ptr<llvm::IRBuilder<>> const& builder,
                    std::shared_ptr<rill::code_generator::llvm_ir_generator::env_id_llvm_table> const& llvm_table,
                    std::vector<rill::environment_id_t> const& parameter_variable_decl_env_ids
                    ) const
                    -> llvm::Value*
                {
                    return builder->CreateSub( llvm_table->ref_value( parameter_variable_decl_env_ids[0] ), llvm_table->ref_value( parameter_variable_decl_env_ids[1] ) );
                }
            };

            construct_predefined_function<operator_sub_action>( embedded_function_action, root_env, operator_sub, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :int, :int )
                std::cout << "add operator -!!" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int
                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int

                return fenv;
            }, int_class_env_pointer );
        }



        {
            //
            // def /( :int, :int ): int => native
            //

            struct operator_divide_action
                : rill::embedded_function_action_base
            {
                // for debug interpreter
                auto invoke( rill::processing_context::debug_interpreter_tag, rill::interpreter::context_ptr const& context ) const
                    -> rill::ast::intrinsic::value_base_ptr
                {
                    auto const& args = context->current_scope()->get_parameter_variable();
                    std::cout << "operator + ! :: args_num -> " << args.size() << std::endl;

                    std::cout << *args[0] << "-" << *args[1] << std::endl;

                    return std::make_shared<rill::ast::intrinsic::int32_value>(
                            std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[0] )->get_value()
                            / std::dynamic_pointer_cast<rill::ast::intrinsic::int32_value const>( args[1] )->get_value()
                            );
                }

                auto invoke(
                    rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                    std::shared_ptr<llvm::Module> const& module,
                    std::shared_ptr<llvm::IRBuilder<>> const& builder,
                    std::shared_ptr<rill::code_generator::llvm_ir_generator::env_id_llvm_table> const& llvm_table,
                    std::vector<rill::environment_id_t> const& parameter_variable_decl_env_ids
                    ) const
                    -> llvm::Value*
                {
                    // signed div
                    return builder->CreateSDiv( llvm_table->ref_value( parameter_variable_decl_env_ids[0] ), llvm_table->ref_value( parameter_variable_decl_env_ids[1] ) );
                }
            };

            construct_predefined_function<operator_divide_action>( embedded_function_action, root_env, operator_divide, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :int, :int )
                std::cout << "add operator /!!" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int
                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, int_class_env_pointer );    // :int

                return fenv;
            }, int_class_env_pointer );
        }











        {
            //
            // def print( :string ): int => native
            //
            struct print_action
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

                auto invoke(
                    rill::processing_context::llvm_ir_generator_tag, // Tag for Rill's LLVM IR Generator
                    std::shared_ptr<llvm::Module> const& module,
                    std::shared_ptr<llvm::IRBuilder<>> const& builder,
                    std::shared_ptr<rill::code_generator::llvm_ir_generator::env_id_llvm_table> const& llvm_table,
                    std::vector<rill::environment_id_t> const& parameter_variable_decl_env_ids
                    ) const
                    -> llvm::Value*
                {
                    // define paramter and return types
                    std::vector<llvm::Type*> const parmeter_types = { llvm::Type::getInt8Ty( llvm::getGlobalContext() )->getPointerTo() };
                    auto const& return_type = llvm::Type::getVoidTy( llvm::getGlobalContext() );

                    // get function type
                    llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parmeter_types, false/*is not variadic*/ );

                    //
                    llvm::Constant* const puts_func = module->getOrInsertFunction( "put_string", func_type );
                    builder->CreateCall( puts_func, llvm_table->ref_value( parameter_variable_decl_env_ids[0] ) );
                    return builder->CreateRetVoid();

                    return builder->CreateCall( puts_func, llvm_table->ref_value( parameter_variable_decl_env_ids[0] ) );
                }
            };
            construct_predefined_function<print_action>( embedded_function_action, root_env, print, [&]( rill::function_symbol_environment_ptr const& fenv ) {
                // ( :string )
                std::cout << "add print" << std::endl;

                fenv->parameter_variable_construct( /*TODO: add attributes, */ nullptr, string_class_env_pointer ); // :string

                return fenv;
            }, void_class_env_pointer );
        }
    }


    //
    // syntax analysis
    //

    std::string const f = ( argc > 1 ) ? argv[1] : "input.rill";

    // first(lexical & syntax)
    std::ifstream ifs( f );
    if ( !ifs ) {
        std::cerr << f << " was not found..." << std::endl;
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
    sample( argc, argv );
}
