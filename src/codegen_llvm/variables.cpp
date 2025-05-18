#include <iostream>
#include <memory>
#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/scope.hpp"
#include "codegen_llvm/types.hpp"
#include "codegen_llvm/values.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>

llvm::GlobalVariable *createStringForGlobalVariable(
    ASTNodePtr initializer,
    llvm::LLVMContext &context_,
    std::unique_ptr<llvm::Module> &module_);

void CodeGenLLVM_Module::compileGlobalVariableDeclaration(ASTNodePtr node)
{
    ASTGlobalVariableDeclaration *varDecl = static_cast<ASTGlobalVariableDeclaration *>(node);
    ASTAccessSpecifier accessSpecifier = varDecl->getAccessSpecifier();
    std::string varName = varDecl->getName();

    std::shared_ptr<CodeGenLLVM_Type> codegenType = nullptr;
    llvm::Type *llvmType = nullptr;
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::InternalLinkage;

    llvm::Constant *constInit = nullptr;
    bool isConstType = false;

    if (varDecl->getTypeValue().has_value())
    {
        ASTTypeSpecifier *varType = static_cast<ASTTypeSpecifier *>(varDecl->getTypeValue().value());
        codegenType = compileType(varType);
        llvmType = codegenType->getLLVMType();
        isConstType = codegenType->isConst();
    }

    if (varDecl->getInitializer().has_value())
    {
        // cannot use `CreateGlobalStringPtr` to create a string literal
        // because it will be a global variable, not a string literal
        // that is why we override string construction here.
        if (varDecl->getInitializer().value()->getType() == ASTNode::NodeType::StringLiteral)
        {
            constInit = createStringForGlobalVariable(varDecl->getInitializer().value(), context_, module_);
            llvmType = llvm::PointerType::get(context_, 0);
        }
        else
        {
            auto init = compileExpr(std::nullopt, varDecl->getInitializer().value());
            constInit = llvm::dyn_cast<llvm::Constant>(init->asValue()->getLLVMValue());
            if (!constInit)
            {
                std::cerr << "(Error) Global variable initializer must be a constant." << std::endl;
                exit(1);
            }
        }
    }

    if (!varDecl->getTypeValue().has_value() && constInit == nullptr)
    {
        std::cerr << "(Error) Global variable type is not specified and initializer is not a constant." << std::endl;
        exit(1);
    }
    else
    {
        // determine variable type from initializer
        llvmType = constInit->getType();
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
    auto threadLocalMode = llvm::GlobalVariable::ThreadLocalMode::NotThreadLocal;

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        *module_,
        llvmType,
        isConstType,
        linkage,
        constInit,
        varName,
        nullptr,
        threadLocalMode,
        0);

    if (publicSymbol)
    {
        // TODO Add to exported symbols
    }
    else
    {
        // TODO Add to local symbols
    }
}

llvm::GlobalVariable *createStringForGlobalVariable(
    ASTNodePtr initializer,
    llvm::LLVMContext &context_,
    std::unique_ptr<llvm::Module> &module_)
{
    ASTStringLiteral *stringLiteral = static_cast<ASTStringLiteral *>(initializer);
    llvm::Constant *strConstant = llvm::ConstantDataArray::getString(context_, stringLiteral->getValue(), true);
    llvm::GlobalVariable *strVar = new llvm::GlobalVariable(
        *module_.get(),
        strConstant->getType(),
        true,
        llvm::GlobalValue::PrivateLinkage,
        strConstant,
        ".str");

    strVar->setAlignment(llvm::MaybeAlign(1));
    strVar->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return strVar;
}

void CodeGenLLVM_Module::compileVariableDeclaration(OptionalScopePtr scopeOpt, ASTNodePtr nodePtr)
{
    SCOPE_REQUIRED

    ASTVariableDeclaration *varDecl = static_cast<ASTVariableDeclaration *>(nodePtr);

    std::shared_ptr<CodeGenLLVM_Type> codegenType = nullptr;
    llvm::AllocaInst *alloca = nullptr;
    std::shared_ptr<CodeGenLLVM_EValue> initializer = nullptr;

    if (varDecl->getTypeValue().has_value())
    {
        ASTTypeSpecifier *varType = static_cast<ASTTypeSpecifier *>(varDecl->getTypeValue().value());
        codegenType = compileType(varType);

        if (varDecl->getInitializer().has_value())
        {
            initializer = compileExpr(scopeOpt, varDecl->getInitializer().value());
        }
    }
    else
    {
        if (varDecl->getInitializer().has_value())
        {
            initializer = compileExpr(scopeOpt, varDecl->getInitializer().value());
            codegenType = initializer->asValue()->getValueType();
        }
        else
        {
            std::cerr << "(Error) Variable type is not specified and initializer is not a constant." << std::endl;
            std::cerr << "        Compiler confused how to build alloca instruction for the variable declaration." << std::endl;
            exit(1);
        }
    }

    std::optional<llvm::Value *> initializerValue = std::nullopt;
    if (initializer)
    {
        initializerValue = initializer->asValue()->getLLVMValue();
    }
    alloca = createZeroInitializedAlloca(varDecl->getName(), codegenType, initializerValue);

    // TODO Jump into appropriate block of the function to declare the variable correctly.
    // builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());

    // TODO Check if the variable is already declared in the current scope

    // add variable to local scope
    auto allocaInnerType = CodeGenLLVM_Type::createPointerType(codegenType);
    auto value = std::make_shared<CodeGenLLVM_Value>(alloca, allocaInnerType);
    auto evalue = std::make_shared<CodeGenLLVM_EValue>(value, CodeGenLLVM_EValue::ValueCategory::LValue);
    SCOPE->setRecord(varDecl->getName(), evalue);
}