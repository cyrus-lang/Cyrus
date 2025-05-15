#include <iostream>
#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"

void CodeGenLLVM_Module::compileGlobalVariableDeclaration(ASTNodePtr node)
{
    ASTGlobalVariableDeclaration *varDecl = static_cast<ASTGlobalVariableDeclaration *>(node);
    ASTTypeSpecifier *varType = static_cast<ASTTypeSpecifier *>(varDecl->getTypeValue());
    ASTAccessSpecifier accessSpecifier = varDecl->getAccessSpecifier();
    std::string varName = varDecl->getName();

    CodeGenLLVM_Type *codegenType = compileType(varType);
    llvm::Type *llvmType = codegenType->getLLVMType();
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::InternalLinkage;

    llvm::Value *init = compileExpr(varDecl->getInitializer());
    llvm::Constant *constInit;
    if (init != nullptr)
    {
        constInit = llvm::dyn_cast<llvm::Constant>(init);
        if (init && !constInit)
        {
            std::cerr << "(Error) Global variable initializer must be a constant." << std::endl;
            exit(1);
        }
    }

    bool publicSymbol = false;

    if (accessSpecifier == ASTAccessSpecifier::Default || accessSpecifier == ASTAccessSpecifier::Private)
    {
        linkage = llvm::GlobalValue::InternalLinkage;
    }
    else if (accessSpecifier == ASTAccessSpecifier::Public)
    {
        publicSymbol = true;
        linkage = llvm::GlobalValue::ExternalLinkage;
    }
    else
    {
        std::cerr << "(Error) Unsupported access specifier for global variable: " << formatAccessSpecifier(accessSpecifier) << std::endl;
        exit(1);
    }

    if (varDecl->getStorageClassSpecifier().has_value())
    {
        ASTStorageClassSpecifier storageClassSpecifier = varDecl->getStorageClassSpecifier().value();

        if (storageClassSpecifier == ASTStorageClassSpecifier::Extern)
        {
            if (constInit != nullptr)
            {
                std::cerr << "(Error) Extern storage class specifier cannot have an initializer." << std::endl;
                exit(1);
            }

            linkage = llvm::GlobalValue::ExternalLinkage;
        }
        else if (storageClassSpecifier == ASTStorageClassSpecifier::Inline)
        {
            std::cerr << "(Error) Inline storage class specifier is not supported for global variables." << std::endl;
            exit(1);
        }
    }

    // REVIEW Consider to make it smarter when adding multi-threading features.
    //
    auto threadLocalMode = llvm::GlobalVariable::ThreadLocalMode::NotThreadLocal;

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        *module_,
        llvmType,
        codegenType->isConst(),
        linkage,
        constInit,
        varName,
        nullptr,
        threadLocalMode,
        0);

    // globalVar->setAlignment(llvm::Align(varDecl->getAlignment()));

    if (publicSymbol)
    {
        // TODO Add to exported symbols
    }
    else
    {
        // TODO Add to local symbols
    }
}
