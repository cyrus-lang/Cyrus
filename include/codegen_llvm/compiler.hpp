#ifndef CODEGEN_LLVM_HPP
#define CODEGEN_LLVM_HPP

#include <llvm/IR/LLVMContext.h>
#include "llvm/IR/Module.h"
#include "ast/ast.hpp"
#include "options.hpp"

void new_codegen_llvm(CodeGenLLVM_Options);

class CodeGenLLVM_Context
{
private:
    llvm::LLVMContext context_;
    std::vector<std::unique_ptr<llvm::Module>> modules_;

public:
    CodeGenLLVM_Context() {}

    llvm::LLVMContext &getContext()
    {
        return context_;
    }

    const std::vector<std::unique_ptr<llvm::Module>> &getModules() const
    {
        return modules_;
    }

    llvm::Module *createModule(const std::string &moduleName)
    {
        auto module = std::make_unique<llvm::Module>(moduleName, context_);
        llvm::Module *modulePtr = module.get();
        modules_.push_back(std::move(module));
        return modulePtr;
    }

    void compileProgram(ASTProgram *program)
    {
        // TODO Get explicit module name declaration from the program.

        delete program;
    }
};

#endif // CODEGEN_LLVM_HPP
