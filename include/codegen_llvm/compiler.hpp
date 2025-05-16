#ifndef CODEGEN_LLVM_HPP
#define CODEGEN_LLVM_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Host.h"
#include <llvm/IR/LLVMContext.h>
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "ast/ast.hpp"
#include "options.hpp"
#include "values.hpp"
#include "types.hpp"
#include <map>

void new_codegen_llvm(CodeGenLLVM_Options);

class CodeGenLLVM_Module
{
private:
    std::unique_ptr<llvm::Module> module_;
    llvm::LLVMContext &context_;
    llvm::IRBuilder<> builder_;
    std::string filePath_;

public:
    CodeGenLLVM_Module(llvm::LLVMContext &context, const std::string &moduleName, const std::string &filePath)
        : module_(std::make_unique<llvm::Module>(moduleName, context)), context_(context), builder_(context), filePath_(filePath)
    {
    }

    llvm::Module *getModule() { return module_.get(); }
    llvm::LLVMContext &getContext() { return context_; }
    void buildProgramIR(ASTProgram *program);
    const std::string &getFilePath() const { return filePath_; }

    // Types
    CodeGenLLVM_Type *compileType(ASTNodePtr nodePtr);

    // Statements
    void compileStmt(ASTNodePtr nodePtr);
    void compileStmts(ASTNodeList nodeList);
    void compileFunctionDefinition(ASTNodePtr nodePtr);
    void compileGlobalVariableDeclaration(ASTNodePtr nodePtr);
    void compileVariableDeclaration(ASTNodePtr nodePtr);

    // Expressions
    CodeGenLLVM_Value compileExpr(ASTNodePtr nodePtr);
    CodeGenLLVM_Value compileIntegerLiteral(ASTNodePtr nodePtr);
    CodeGenLLVM_Value compileFloatLiteral(ASTNodePtr nodePtr);
    CodeGenLLVM_Value compileStringLiteral(ASTNodePtr nodePtr);
    CodeGenLLVM_Value compileBoolLiteral(ASTNodePtr nodePtr);
};

class CodeGenLLVM_Context
{
private:
    llvm::LLVMContext context_;
    std::map<std::string, CodeGenLLVM_Module *> modules_;
    llvm::TargetMachine *targetMachine_;
    std::string triple_;

public:
    CodeGenLLVM_Context()
    {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();

        triple_ = llvm::sys::getDefaultTargetTriple();

        std::string error;
        const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple_, error);
        if (!target)
        {
            llvm::errs() << "(Error) LLVM Failed to find target: " << error << "\n";
            exit(1);
        }

        llvm::TargetOptions options;
        auto RM = std::optional<llvm::Reloc::Model>();
        llvm::TargetMachine *targetMachine = target->createTargetMachine(
            triple_,
            "generic",
            "",
            options,
            RM);

        targetMachine_ = targetMachine;
    }
    ~CodeGenLLVM_Context()
    {
        for (auto &pair : modules_)
        {
            delete pair.second;
        }
        delete targetMachine_;
    }

    llvm::LLVMContext &getContext() { return context_; }
    const std::map<std::string, CodeGenLLVM_Module *> &getModules() const { return modules_; }

    CodeGenLLVM_Module *createModule(const std::string &moduleName, const std::string &filePath)
    {
        CodeGenLLVM_Module *module = new CodeGenLLVM_Module(context_, moduleName, filePath);
        module->getModule()->setTargetTriple(triple_);
        module->getModule()->setDataLayout(targetMachine_->createDataLayout());

        modules_.emplace(moduleName, module);
        return modules_.at(moduleName);
    }

    void saveIR(const std::string &outputPath);
};

#endif // CODEGEN_LLVM_HPP