//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/code_generator/binary_generator_from_llvm_ir.hpp>

#include <string>
#include <iostream>

#include <llvm/ADT/Triple.h>

#include <llvm/Assembly/PrintModulePass.h>

#include <llvm/CodeGen/LinkAllAsmWriterComponents.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Module.h>

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
        struct llvm_initializer
        {
            llvm_initializer()
            {
//                llvm::InitializeAllTargets();
                {
                    LLVMInitializeX86TargetInfo();
                    LLVMInitializeX86Target();
                }
//                llvm::InitializeAllTargetMCs();
                {
                    LLVMInitializeX86TargetMC();
                }
//                llvm::InitializeAllAsmPrinters();
                {
                    LLVMInitializeX86AsmPrinter();
                }
//                llvm::InitializeAllAsmParsers();
                {
                    LLVMInitializeX86AsmParser();
                }

                llvm::PassRegistry& registry = *llvm::PassRegistry::getPassRegistry();
                llvm::initializeCore( registry );
                llvm::initializeCodeGen( registry );
                llvm::initializeLoopStrengthReducePass( registry );
                llvm::initializeLowerIntrinsicsPass( registry );
                llvm::initializeUnreachableBlockElimPass( registry );
            }
        };


        void binary_generator_from_llvm_ir::test( llvm::Module& module ) const
        {
            static llvm_initializer const li;   // initialize LLVM at once

            // Triple for current system
            llvm::Triple triple( llvm::sys::getDefaultTargetTriple() );


            std::string const arch_name = "";
            std::string error_message;

            // lookup target by arch_name, and it failed, triple value will be used
            llvm::Target const* target = llvm::TargetRegistry::lookupTarget( arch_name, triple, error_message );
            if ( !target ) {
                assert( false );
            }

            //
            std::string cpu_name = "";

            //
            std::string features = "";

            //
            llvm::TargetOptions options;

            //
            llvm::Reloc::Model reloc_model = llvm::Reloc::Default;

            //
            llvm::CodeModel::Model code_model = llvm::CodeModel::Default;

            //
            llvm::CodeGenOpt::Level opt_level = llvm::CodeGenOpt::Default;

            //
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
                std::cerr << "couldn't create target machine";
                assert( false );
            }

            std::cout << "triple : " << triple.getTriple() << std::endl;



            // Build up all of the passes that we want to do to the module.
            llvm::PassManager PM;

            // Add an appropriate TargetLibraryInfo pass for the module's triple.
            llvm::TargetLibraryInfo* TLI = new llvm::TargetLibraryInfo( triple );
            PM.add(TLI);

            // Add intenal analysis passes from the target machine.
            target_machine->addAnalysisPasses(PM);

            // Add the target data from the target machine, if it exists, or the module.
            if ( const llvm::DataLayout *TD = target_machine->getDataLayout() ) {
                PM.add( new llvm::DataLayout( *TD ) );
                std::cout << "const llvm::DataLayout *TD = target_machine->getDataLayout()" << std::endl;
            } else {
                PM.add( new llvm::DataLayout( &module ) );
            }


            {
                // Open the file.
                std::string error;
                llvm::sys::fs::OpenFlags const OpenFlags = llvm::sys::fs::F_None | llvm::sys::fs::F_Binary;
                llvm::tool_output_file FDOut( "out.obj", error, OpenFlags );

                llvm::TargetMachine::CodeGenFileType FileType = llvm::TargetMachine::CGFT_ObjectFile;

                std::cout << target_machine.get() << std::endl;

                llvm::formatted_raw_ostream FOS( FDOut.os() );
                if ( target_machine->addPassesToEmitFile( PM, FOS, FileType ) ) {
                    std::cout << "failed" << std::endl;
                    //return;
                }
                PM.run( module );

                // Declare success.
                FDOut.keep();
            }


#if 0
            // !!! DEBUG !!!
            // TODO: change to more geniric process
            {
                std::string const command = "\"C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\bin\\link.exe\" "\
                    "/ENTRY:rill_main "\
                    "/NOLOGO /MACHINE:X86 /SUBSYSTEM:CONSOLE " \
                    "/NODEFAULTLIB /OUT:a.exe out.obj ..\\Debug\\rillrt.lib";
                std::system( command.c_str() );
            }
#endif

            {
                std::string const command = "ld " \
                    "-e rill_main "\
                    /*"/NOLOGO /MACHINE:X86 /SUBSYSTEM:CONSOLE "*/      \
                    /*"-nodefaultlibs"*/" -dynamic-linker /lib64/ld-linux-x86-64.so.2 -lc -o a.out out.obj /usr/local/lib/librill-rt.a";
                std::system( command.c_str() );
                std::string const command2 = "a.out";
                std::system( command2.c_str() );
            }

#if 0
            // !!! DEBUG !!!
            {
                std::string const command = "a.exe";
                std::system( command.c_str() );
            }
#endif

            std::cout << std::endl;
        }
    } // namespace code_generator
} // namespace rill
