//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/llvm_engine/support.hpp>

#include <string>
#include <iostream>

#include <llvm/ADT/Triple.h>

#include <llvm/CodeGen/LinkAllAsmWriterComponents.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Module.h>
#include <llvm/Assembly/PrintModulePass.h> // will be changed #if ( LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 5 )

#include <llvm/IRReader/IRReader.h>

#include <llvm/MC/SubtargetFeature.h>

#include <llvm/Pass.h>
#include <llvm/PassManager.h>

//#include <llvm/Support/Debug.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/Host.h>
//#include <llvm/Support/ManagedStatic.h>
//#include <llvm/Support/PluginLoader.h>
//#include <llvm/Support/PrettyStackTrace.h>
//#include <llvm/Support/Signals.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/ToolOutputFile.h>

#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Target/TargetMachine.h>


namespace rill
{
    namespace code_generator
    {
        namespace llvm_engine
        {
            namespace detail
            {
                struct llvm_initializer
                {
                    llvm_initializer()
                    {
                        // llvm::InitializeAllTargets();
                        {
                            LLVMInitializeX86TargetInfo();
                            LLVMInitializeX86Target();
                        }
                        // llvm::InitializeAllTargetMCs();
                        {
                            LLVMInitializeX86TargetMC();
                        }
                        // llvm::InitializeAllAsmPrinters();
                        {
                            LLVMInitializeX86AsmPrinter();
                        }
                        // llvm::InitializeAllAsmParsers();
                        {
                            LLVMInitializeX86AsmParser();
                        }

                        // for jit
                        llvm::InitializeNativeTarget();

                        //
                        llvm::PassRegistry& registry = *llvm::PassRegistry::getPassRegistry();
                        llvm::initializeCore( registry );
                        llvm::initializeCodeGen( registry );
                        llvm::initializeLoopStrengthReducePass( registry );
                        llvm::initializeLowerIntrinsicsPass( registry );
                        llvm::initializeUnreachableBlockElimPass( registry );
                    }
                };

                auto initialize_llvm()
                    -> void
                {
                    static auto const li = llvm_initializer();   // initialize LLVM at once
                }

            } // namespace detail

            //
            auto make_default_target_machine( std::string& error_log )
                -> std::unique_ptr<llvm::TargetMachine>
            {
                detail::initialize_llvm();

                // Triple for current system
                llvm::Triple triple( llvm::sys::getDefaultTargetTriple() );

                error_log.clear();

                // lookup target by arch_name, and it failed, triple value will be used
                //  will be fixed in LLVM 3.5
                std::string const arch_name = "";
                llvm::Target const* target = llvm::TargetRegistry::lookupTarget( arch_name, triple, error_log );
                if ( !target ) {
                    return nullptr;
                }

                std::string cpu_name = "";
                std::string features = "";
                llvm::TargetOptions options;
                llvm::Reloc::Model reloc_model = llvm::Reloc::Default;
                llvm::CodeModel::Model code_model = llvm::CodeModel::Default;
                llvm::CodeGenOpt::Level opt_level = llvm::CodeGenOpt::Default;

                error_log.clear();
                std::unique_ptr<llvm::TargetMachine> target_machine(
                    target->createTargetMachine(
                        triple.getTriple(),
                        cpu_name,
                        features,
                        options,
                        reloc_model,
                        code_model,
                        opt_level
                        )
                    );
                if ( !target_machine ) {
                    error_log = "couldn't create target machine";
                    return nullptr;
                }

                return target_machine;
            }

        } // namespace llvm_engine
    } // namespace code_generator
} // namespace rill
