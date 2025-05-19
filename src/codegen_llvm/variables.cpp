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

    std::shared_ptr<CodeGenLLVM_EValue> initializer = nullptr;
    std::shared_ptr<CodeGenLLVM_Type> codegenType = nullptr;
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::InternalLinkage;
    bool isConstType = false;

    if (globalVarTable_.find(varName) != globalVarTable_.end())
    {
        DISPLAY_DIAG(varDecl->getLineNumber(), "Global variable '" + varName + "' is already defined in this module.");
    }

    if (varDecl->getTypeValue().has_value())
    {
        ASTTypeSpecifier *varType = static_cast<ASTTypeSpecifier *>(varDecl->getTypeValue().value());
        codegenType = compileType(varType);
        isConstType = codegenType->isConst();
    }

    if (varDecl->getInitializer().has_value())
    {
        // cannot use `CreateGlobalStringPtr` to create a string literal
        // because it will be a global variable, not a string literal
        // that is why we override string construction here.
        if (varDecl->getInitializer().value()->getType() == ASTNode::NodeType::StringLiteral)
        {
            // constInit = createStringForGlobalVariable(varDecl->getInitializer().value(), context_, module_);
            // codegenType = compileType(varDecl->getInitializer().value());
            // FIXME
        }
        else
        {
            initializer = compileExpr(std::nullopt, varDecl->getInitializer().value());
        }
    }

    if (!varDecl->getTypeValue().has_value())
    {
        if (initializer)
        {
            // determine variable type from initializer
            codegenType = initializer->asValue()->getValueType();
            isConstType = codegenType->isConst();
        }
        else
        {
            DISPLAY_DIAG(varDecl->getLineNumber(), "Global variable type is not specified and initializer is not a constant.");
        }
    }

    bool exported = false;

    if (accessSpecifier == ASTAccessSpecifier::Default || accessSpecifier == ASTAccessSpecifier::Private)
    {
        linkage = llvm::GlobalValue::InternalLinkage;
    }
    else if (accessSpecifier == ASTAccessSpecifier::Public)
    {
        exported = true;
        linkage = llvm::GlobalValue::ExternalLinkage;
    }
    else
    {
        DISPLAY_DIAG(varDecl->getLineNumber(), "Unsupported access specifier for global variable: " + formatAccessSpecifier(accessSpecifier));
    }

    if (varDecl->getStorageClassSpecifier().has_value())
    {
        ASTStorageClassSpecifier storageClassSpecifier = varDecl->getStorageClassSpecifier().value();

        if (storageClassSpecifier == ASTStorageClassSpecifier::Extern)
        {
            if (initializer != nullptr)
            {
                DISPLAY_DIAG(varDecl->getLineNumber(), "Extern storage class specifier cannot have an initializer.");
            }

            linkage = llvm::GlobalValue::ExternalLinkage;
        }
        else if (storageClassSpecifier == ASTStorageClassSpecifier::Inline)
        {   
            DISPLAY_DIAG(varDecl->getLineNumber(), "Inline storage class specifier is not supported for global variables.");
        }
    }

    llvm::Value *valueInitializer = nullptr;
    if (initializer)
    {
        valueInitializer = initializer->asValue()->getLLVMValue();
    }
    else
    {
        valueInitializer = createZeroInitializedValue(codegenType, varDecl->getLineNumber());
    }

    llvm::Constant *constantInitializer = llvm::dyn_cast<llvm::Constant>(valueInitializer);
    if (!constantInitializer)
    {
        DISPLAY_DIAG(varDecl->getLineNumber(), "Global variable initializer is not a constant.");
    }

    // REVIEW Consider to make it smarter when adding multi-threading features.
    auto threadLocalMode = llvm::GlobalVariable::ThreadLocalMode::NotThreadLocal;

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        *module_,
        codegenType->getLLVMType(),
        isConstType,
        linkage,
        constantInitializer,
        varName,
        nullptr,
        threadLocalMode,
        0);

    globalVarTable_[varName] = GlobalVarTableItem(globalVar, codegenType, exported);
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
    ASTVariableDeclaration *varDecl = static_cast<ASTVariableDeclaration *>(nodePtr);
    SCOPE_REQUIRED(varDecl->getLineNumber());

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
            DISPLAY_DIAG(varDecl->getLineNumber(), "Variable type is not specified and initializer is not a constant.");
        }
    }

    std::optional<llvm::Value *> initializerValue = std::nullopt;
    if (initializer)
    {
        initializerValue = initializer->asValue()->getLLVMValue();
    }

    alloca = createZeroInitializedAlloca(varDecl->getName(), codegenType, initializerValue, varDecl->getLineNumber());

    // TODO Jump into appropriate block of the function to declare the variable correctly.
    // builder.SetInsertPoint(&func->getEntryBlock(), func->getEntryBlock().begin());

    if (SCOPE->getRecord(varDecl->getName()).has_value())
    {   
        DISPLAY_DIAG(varDecl->getLineNumber(), "Variable '" + varDecl->getName() + "' is already declared in the current scope.");
    }

    // add variable to local scope
    auto allocaInnerType = CodeGenLLVM_Type::createPointerType(codegenType);
    auto value = std::make_shared<CodeGenLLVM_Value>(alloca, allocaInnerType);
    auto evalue = std::make_shared<CodeGenLLVM_EValue>(value, CodeGenLLVM_EValue::ValueCategory::LValue);
    SCOPE->setRecord(varDecl->getName(), evalue);
}