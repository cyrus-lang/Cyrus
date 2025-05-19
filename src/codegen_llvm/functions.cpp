#include <memory>
#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/types.hpp"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#define DEFAULT_FUNCTION_LINKAGE llvm::GlobalValue::LinkageTypes::InternalLinkage

void CodeGenLLVM_Module::compileFunctionDefinition(ASTNodePtr node)
{
    Scope* scope = new Scope();

    ASTFunctionDefinition *funcDef = static_cast<ASTFunctionDefinition *>(node);
    std::string funcName = static_cast<ASTIdentifier *>(funcDef->getExpr())->getName();
    ASTFunctionParameters params = funcDef->getParameters();
    std::optional<ASTStorageClassSpecifier> storageClass = funcDef->getStorageClassSpecifier();
    ASTStatementList *body = static_cast<ASTStatementList *>(funcDef->getBody());
    bool exported = false;

    if (funcTable_.find(funcName) != funcTable_.end()) {
        std::cerr << "(Error) Function '" << funcName << "' is already defined in this module." << std::endl;
        exit(1);
    }

    llvm::Type *returnType;
    if (funcDef->getReturnType().has_value())
    {
        std::shared_ptr<CodeGenLLVM_Type> codegenType = compileType(funcDef->getReturnType().value());
        returnType = codegenType->getLLVMType();
    }
    else
    {
        returnType = llvm::Type::getVoidTy(context_);
    }

    // Function parameters
    std::vector<llvm::Type *> paramTypes;
    bool isVariadic = false;

    // Function linkage

    if (storageClass.has_value() && storageClass.value() == ASTStorageClassSpecifier::Extern)
    {
        std::cerr << "Function definition cannot get an extern storage class." << std::endl;
        exit(1);
    }

    llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, paramTypes, isVariadic);
    llvm::Function *func = llvm::Function::Create(funcType, DEFAULT_FUNCTION_LINKAGE, funcName, module_.get());

    if (storageClass.has_value() && storageClass.value() == ASTStorageClassSpecifier::Inline)
    {
        if (funcDef->getAccessSpecifier() == ASTAccessSpecifier::Public)
        {
            func->setLinkage(llvm::GlobalValue::LinkageTypes::AvailableExternallyLinkage);
        }
        else
        {
            func->setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
        }

        func->addFnAttr(llvm::Attribute::AlwaysInline);
    }
    else if (funcDef->getAccessSpecifier() == ASTAccessSpecifier::Public)
    {
        exported = true;
        func->setLinkage(llvm::GlobalValue::LinkageTypes::ExternalLinkage);
    }

    // Construct function body

    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(context_, "entry", func);
    builder_.SetInsertPoint(entryBlock);

    compileStmts(scope, body->getStatements());

    // TODO
    // Track block termination for function.
    // But before achieve this functionality, add function to func_table.

    // add to func table
    funcTable_[funcName] = FuncTableItem(func, params, exported);

    delete scope;
}