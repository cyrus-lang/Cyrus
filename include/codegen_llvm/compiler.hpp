#ifndef CODEGEN_LLVM_HPP
#define CODEGEN_LLVM_HPP

#include <map>
#include <llvm/IR/LLVMContext.h>
#include "llvm/IR/Module.h"
#include "ast/ast.hpp"
#include "options.hpp"

void new_codegen_llvm(CodeGenLLVM_Options);

class CodeGenLLVM_Module
{
private:
    std::unique_ptr<llvm::Module> module_;
    llvm::LLVMContext &context_;
    std::string filePath_;

public:
    CodeGenLLVM_Module(llvm::LLVMContext &context, const std::string &moduleName)
        : module_(std::make_unique<llvm::Module>(moduleName, context)), context_(context), filePath_("") {}

    llvm::Module *getModule() { return module_.get(); }
    llvm::LLVMContext &getContext() { return context_; }
    void compileProgram(ASTProgram *program);
    const std::string &getFilePath() const { return filePath_; }
};

class CodeGenLLVM_Context
{
private:
    llvm::LLVMContext context_;
    std::map<std::string, CodeGenLLVM_Module*> modules_;

public:
    explicit CodeGenLLVM_Context() {}
    llvm::LLVMContext &getContext() { return context_; }
    const std::map<std::string, CodeGenLLVM_Module*> &getModules() const { return modules_; }

    CodeGenLLVM_Module *createModule(const std::string &moduleName)
    {
        modules_.emplace(moduleName, new CodeGenLLVM_Module(context_, moduleName));
        return modules_.at(moduleName);
    }
};

#endif // CODEGEN_LLVM_HPP