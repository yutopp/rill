//
// Copyright yutopp 2017 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>
#include <memory>
#include <string>
#include <caml/mlvalues.h>
#include <llvm-c/Core.h>    // LLVMModuleRef
#include <llvm/CodeGen/Passes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/TargetLibraryInfo.h>

// implementations
// Regards all initializations for targets have been done in ocaml world
namespace detail {
    //
    void initialize_codegen()
    {
        llvm::PassRegistry& pass_registry = *llvm::PassRegistry::getPassRegistry();
        llvm::initializeCodeGen(pass_registry);
    }

    //
    void emit_file_for_target_machine(llvm::Module& m, char const* const output_path)
    {
        auto const arch_name = "";
        auto triple = llvm::Triple(m.getTargetTriple());

        std::string error_msg;
        llvm::Target const* target =
            llvm::TargetRegistry::lookupTarget(arch_name, triple, error_msg);
        if (error_msg != "") {
            std::cerr << "error: " << error_msg << std::endl;
        }

        //
        std::string cpu_name = "";
        std::string features = "";
        llvm::TargetOptions options;
        llvm::Reloc::Model reloc_model = llvm::Reloc::PIC_;
        llvm::CodeModel::Model code_model = llvm::CodeModel::Default;
        llvm::CodeGenOpt::Level opt_level = llvm::CodeGenOpt::Default;

        //
        auto target_machine =
            target->createTargetMachine(triple.str(),
                                        cpu_name,
                                        features,
                                        options,
                                        reloc_model,
                                        code_model,
                                        opt_level);
        if (target_machine == nullptr) {
            std::cerr << "couldn't create target machine";
        }

        //
        llvm::legacy::PassManager pm;

        // http://llvm.org/docs/doxygen/html/classllvm_1_1TargetLibraryInfoImpl.html
        // control target library information availability
        auto tlii = llvm::TargetLibraryInfoImpl(triple);
        pm.add(new llvm::TargetLibraryInfoWrapperPass(tlii));

        // TODO: add passes

        //
        auto const open_flags = llvm::sys::fs::F_None; // llvm::sys::fs::F_Text;
        std::error_code ec;
        llvm::raw_fd_ostream out_buf{output_path, ec, open_flags};
        if (ec) {
            std::cerr << "out_buf" << error_msg << std::endl;
        }

        auto const file_type = llvm::TargetMachine::CGFT_ObjectFile;
        if (target_machine->addPassesToEmitFile(pm, out_buf, file_type)) {
            std::cerr << "fail" << std::endl;
        }

        pm.run(m);
    }
}

// ocaml binsings
extern "C" {
    CAMLprim value
    rillc_cg_initialize_llvm_codegen(value _unit)
    {
        detail::initialize_codegen();
        return Val_unit;
    }

    CAMLprim value
    rillc_cg_emit_file_for_target_machine(LLVMModuleRef c_module, value output_path_rep)
    {
        llvm::Module& m = *llvm::unwrap(c_module);
        detail::emit_file_for_target_machine(m, String_val(output_path_rep));

        return Val_unit;
    }
}
