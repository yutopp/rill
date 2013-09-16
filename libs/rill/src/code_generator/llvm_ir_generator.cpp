//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/environment.hpp>

#include <boost/scope_exit.hpp>

#include <llvm/Analysis/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <rill/ast/root.hpp>
#include <rill/ast/statement.hpp>
#include <rill/ast/expression.hpp>
#include <rill/ast/value.hpp>


namespace rill
{
    namespace code_generator
    {
        class llvm_ir_generator::env_id_llvm_table
        {
        public:
            auto bind_value( environment_id_t const& env_id, llvm::Value* const value )
                -> void
            {
                // TODO: dup check
                value_table_.emplace( env_id, value );
            }

            auto ref_value( environment_id_t const& env_id )
                -> llvm::Value*
            {
                return value_table_.at( env_id );
            }

            auto bind_type( environment_id_t const& env_id, llvm::Type* const value )
                -> void
            {
                // TODO: dup check
                type_table_.emplace( env_id, value );
            }

            auto ref_type( environment_id_t const& env_id )
                -> llvm::Type*
            {
                return type_table_.at( env_id );
            }

        private:
            std::unordered_map<environment_id_t, llvm::Value*> value_table_;
            std::unordered_map<environment_id_t, llvm::Type*> type_table_;
        };




        llvm_ir_generator::llvm_ir_generator( const_environment_ptr const& root_env )
            : context_( llvm::getGlobalContext() )
            , module_( std::make_shared<llvm::Module>( "rill", context_ ) )
            , builder_( std::make_shared<llvm::IRBuilder<>>( context_ ) )
            , llvm_table_( std::make_shared<env_id_llvm_table>() )
            , root_env_( root_env )
        {}


        // Root Scope
        RILL_TV_OP_CONST( llvm_ir_generator, ast::root, r, root_env )
        {
            // maybe rooted once

            //
            // bind primitive types
            //

            // bind [ int -> i32 ]
            llvm_table_->bind_type( root_env->lookup( intrinsic::make_single_identifier( "int" ) )->get_id(), llvm::Type::getInt32Ty( llvm::getGlobalContext() ) );

            //
            for( auto const& node : r->statements_ )
                dispatch( node );
        }

        //
        RILL_TV_OP_CONST( llvm_ir_generator, ast::expression_statement, s, env )
        {
            auto const v = dispatch( s->expression_, env );
        }


        RILL_TV_OP_CONST( llvm_ir_generator, ast::return_statement, s, env )
        {
            builder_->CreateRet( dispatch( s->expression_, env ) );
        }


        RILL_TV_OP_CONST( llvm_ir_generator, ast::function_definition_statement, s, env )
        {
            //
            std::cout << "ast::function_definition_statement" << env << " / " << root_env_->get_id() << std::endl;

            //
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( ( env != nullptr ) ? env : root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            auto const& parameter_variable_decl_env_ids = f_env->get_parameter_decl_ids();
            auto const& parameter_variable_type_env_ids = f_env->get_parameter_type_ids();
            std::cout << f_env << std::endl;

            //
            auto const current_insert_point = builder_->saveIP();

            //
            // TODO: use current_insert_point.isSet(), if it is false, this function needs external linkage( currently, all functions are exported as external linkage )
            auto const linkage = llvm::Function::ExternalLinkage;
            if ( current_insert_point.isSet() ) {
                //
                std::cout << "not external." << std::endl;
            }

            // signature
            std::vector<llvm::Type*> parameter_types;
            for( auto const& type_env_id : parameter_variable_type_env_ids ) {
                std::cout << type_env_id << std::endl;
                parameter_types.push_back( llvm_table_->ref_type( type_env_id ) );
            }
            llvm::Type* const return_type = llvm_table_->ref_type( f_env->get_return_type_environment()->get_id() );
            llvm::FunctionType* const func_type = llvm::FunctionType::get( return_type, parameter_types, false/*not variable*/ );
            
            // function body
            llvm::Function* const func = llvm::Function::Create( func_type, linkage, f_env->mangled_name(), module_.get() );
            for( llvm::Function::arg_iterator ait = func->arg_begin(); ait != func->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;
                auto const& var = std::static_pointer_cast<variable_symbol_environment const>( env->get_env_at( parameter_variable_decl_env_ids[ait->getArgNo()] ).lock() );
                ait->setName( var->mangled_name() );

                llvm_table_->bind_value( var->get_id(), ait );
            }

            // create a new basic block to start insertion into.
            llvm::BasicBlock* const basic_brock = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", func );
            builder_->SetInsertPoint( basic_brock );

            //
            for( auto const& node : s->statements_ )
                dispatch( node, root_env_->get_related_env_by_ast_ptr( node ) );

            // restore insert point
            builder_->restoreIP( current_insert_point );
        }

        RILL_TV_OP_CONST( llvm_ir_generator, ast::embedded_function_definition_statement, s, env )
        {
            //auto const current_insert_point = builder_->GetInsertPoint();
            //builder_->SetInsertPoint( current_insert_point );
            //BOOST_SCOPE_EXIT((&builder_)(current_insert_point)) {
            //    builder_->SetInsertPoint( current_insert_point );
            //} BOOST_SCOPE_EXIT_END

            // cast to function symbol env
            auto const& f_env = std::static_pointer_cast<function_symbol_environment const>( ( env != nullptr ) ? env : root_env_->get_related_env_by_ast_ptr( s ) );
            assert( f_env != nullptr );
            auto const& parameter_env_ids = f_env->get_parameter_decl_ids();

            // !!! PRE IMPL !!!

            // Make the function type:  +(int, int): int etc.
            std::vector<llvm::Type*> int_types( 2, llvm::Type::getInt32Ty( llvm::getGlobalContext() ) );
            llvm::FunctionType* const func_type = llvm::FunctionType::get( llvm::Type::getInt32Ty( llvm::getGlobalContext() ), int_types, false );
            
            llvm::Function* const F = llvm::Function::Create( func_type, llvm::Function::ExternalLinkage, f_env->mangled_name(), module_.get() );
            for( llvm::Function::arg_iterator ait = F->arg_begin(); ait != F->arg_end(); ++ait ) {
                std::cout << "Argument No: " << ait->getArgNo() << std::endl;
                auto const& var = std::static_pointer_cast<variable_symbol_environment const>( f_env->get_env_at( parameter_env_ids[ait->getArgNo()] ).lock() );
                ait->setName( var->mangled_name() );

                llvm_table_->bind_value( var->get_id(), ait );
            }
            // Create a new basic block to start insertion into.
            llvm::BasicBlock* const BB = llvm::BasicBlock::Create( llvm::getGlobalContext(), "entry", F );
            builder_->SetInsertPoint(BB);
            builder_->CreateRet( builder_->CreateAdd( llvm_table_->ref_value( parameter_env_ids[0] ), llvm_table_->ref_value( parameter_env_ids[1] ) ) );

            //
            llvm::verifyFunction( *F );
        }

        RILL_TV_OP_CONST( llvm_ir_generator, ast::binary_operator_expression, e, _ )
        {
            // evaluate values(and push to stack) and returned value type
            auto const& rhs_value = dispatch( e->rhs_, _ );
            auto const& lhs_value = dispatch( e->lhs_, _ );
            assert( rhs_value != nullptr && lhs_value != nullptr );

            auto const f_env = std::static_pointer_cast<function_symbol_environment const>( root_env_->get_related_env_by_ast_ptr( e ) );
            assert( f_env != nullptr );
            auto const& target_name = f_env->mangled_name();
            auto const& callee_function = [&]() -> llvm::Function* const {
                if ( auto const& f = module_->getFunction( target_name ) )
                    return f;

                // generate
                dispatch( f_env->get_related_ast(), f_env );
                if ( auto const& f = module_->getFunction( target_name ) )
                    return f;

                return nullptr;
            }();
            if ( !callee_function ) {
                // unexpected error...
                assert( false );
            }

            std::cout << "CALL!!!!!" << std::endl;

            // push values( last to front )
            std::vector<llvm::Value*> args;
            args.push_back( rhs_value );
            args.push_back( lhs_value );

            // invocation
            return builder_->CreateCall( callee_function, args, "calltmp" );
        }

        RILL_TV_OP_CONST( llvm_ir_generator, ast::term_expression, e, _ )
        {
            return dispatch( e->value_, _ );
        }

        RILL_TV_OP_CONST( llvm_ir_generator, ast::intrinsic_value, v, _ )
        {
            // TODO: check primitive type
            // Currently, return int type( 32bit, integer )
            return llvm::ConstantInt::get( llvm::getGlobalContext(), llvm::APInt( 32, std::static_pointer_cast<ast::intrinsic::int32_value const>( v->value_ )->get_value() ) );
        }

    } // namespace code_generator
} // namespace rill


// for MSVC
//
//
#pragma comment( lib, "LLVMTableGen.lib" )

#pragma comment( lib, "LLVMRuntimeDyld.lib" )
#pragma comment( lib, "LLVMObject.lib" )

#pragma comment( lib, "LLVMLinker.lib" )
#pragma comment( lib, "LLVMipo.lib" )
#pragma comment( lib, "LLVMInterpreter.lib" )
#pragma comment( lib, "LLVMInstrumentation.lib" )
#pragma comment( lib, "LLVMJIT.lib" )
#pragma comment( lib, "LLVMExecutionEngine.lib" )
#pragma comment( lib, "LLVMDebugInfo.lib" )
#pragma comment( lib, "LLVMBitWriter.lib" )

#pragma comment( lib, "LLVMAsmParser.lib" )
#pragma comment( lib, "LLVMArchive.lib" )
#pragma comment( lib, "LLVMBitReader.lib" )
#pragma comment( lib, "LLVMSelectionDAG.lib" )
#pragma comment( lib, "LLVMAsmPrinter.lib" )

#pragma comment( lib, "LLVMCodeGen.lib" )
#pragma comment( lib, "LLVMScalarOpts.lib" )
#pragma comment( lib, "LLVMInstCombine.lib" )
#pragma comment( lib, "LLVMTransformUtils.lib" )
#pragma comment( lib, "LLVMipa.lib" )
#pragma comment( lib, "LLVMAnalysis.lib" )
#pragma comment( lib, "LLVMTarget.lib" )
#pragma comment( lib, "LLVMCore.lib" )

#pragma comment( lib, "LLVMSupport.lib" )

#pragma comment( lib, "LLVMX86Disassembler.lib" )
#pragma comment( lib, "LLVMX86AsmParser.lib" )
#pragma comment( lib, "LLVMX86CodeGen.lib" )
#pragma comment( lib, "LLVMX86Desc.lib" )
#pragma comment( lib, "LLVMX86AsmPrinter.lib" )
#pragma comment( lib, "LLVMX86Utils.lib" )
#pragma comment( lib, "LLVMX86Info.lib" )