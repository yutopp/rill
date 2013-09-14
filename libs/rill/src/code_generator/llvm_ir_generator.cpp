//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_ir_generator.hpp>
#include <rill/environment.hpp>

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
        // Root Scope
        RILL_TV_OP( llvm_ir_generator, ast::root, r, env )
        {
            for( auto const& node : r->statements_ )
                dispatch( node, env );
        }

        //
        RILL_TV_OP( llvm_ir_generator, ast::expression_statement, s, env )
        {
            auto const v = dispatch( s->expression_, env );
        }

        RILL_TV_OP( llvm_ir_generator, ast::binary_operator_expression, e, env )
        {
            // evaluate values(and push to stack) and returned value type
            auto const& rhs_value = dispatch( e->rhs_, env );
            auto const& lhs_value = dispatch( e->lhs_, env );
            assert( rhs_value != nullptr && lhs_value != nullptr );



            return nullptr;
        }

        RILL_TV_OP( llvm_ir_generator, ast::term_expression, e, env )
        {
            return dispatch( e->value_, env );
        }

        RILL_TV_OP( llvm_ir_generator, ast::intrinsic_value, v, env )
        {
            // TODO: check primitive type
            // Currently, return int type
            return llvm::ConstantInt::get( llvm::getGlobalContext(), llvm::APInt( std::static_pointer_cast<ast::intrinsic::int32_value>( v->value_ )->get_value(), 10 ) );
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