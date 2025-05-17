#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/types.hpp"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#define DEFAULT_FUNCTION_LINKAGE llvm::GlobalValue::LinkageTypes::InternalLinkage

void CodeGenLLVM_Module::compileFunctionDefinition(ASTNodePtr node)
{
    ASTFunctionDefinition *funcDef = static_cast<ASTFunctionDefinition *>(node);
    std::string functionName = static_cast<ASTIdentifier *>(funcDef->getExpr())->getName();
    ASTFunctionParameters params = funcDef->getParameters();
    std::optional<ASTStorageClassSpecifier> storageClass = funcDef->getStorageClassSpecifier();
    ASTStatementList *body = static_cast<ASTStatementList *>(funcDef->getBody());

    llvm::Type *returnType;
    if (funcDef->getReturnType().has_value())
    {
        CodeGenLLVM_Type *codegenType = compileType(funcDef->getReturnType().value());
        returnType = codegenType->getLLVMType();
        delete codegenType;
    }
    else
    {
        returnType = llvm::Type::getVoidTy(context_);
    }

    // Function parameters

    // TODO
    std::vector<llvm::Type *> funcParams = {};
    bool isVariadic = false;

    // Function linkage

    if (storageClass.has_value() && storageClass.value() == ASTStorageClassSpecifier::Extern)
    {
        std::cerr << "Function definition cannot get an extern storage class." << std::endl;
        exit(1);
    }

    llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, funcParams, isVariadic);
    llvm::Function *func = llvm::Function::Create(funcType, DEFAULT_FUNCTION_LINKAGE, functionName, module_.get());

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
        func->setLinkage(llvm::GlobalValue::LinkageTypes::ExternalLinkage);
    }

    // Construct function body

    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(context_, "entry", func);
    builder_.SetInsertPoint(entryBlock);

    compileStmts(body->getStatements());

    // TODO
    // Track block termination for function.
    // But before achieve this functionality, add function to func_table.
}