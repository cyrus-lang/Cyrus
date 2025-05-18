#include <iostream>
#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/types.hpp"
#include <llvm/IR/IRBuilder.h>
#include "llvm/IR/Type.h"

std::shared_ptr<CodeGenLLVM_Type> CodeGenLLVM_Module::compileType(ASTNodePtr node)
{
    ASTTypeSpecifier *typeSpecifier = static_cast<ASTTypeSpecifier *>(node);
    switch (typeSpecifier->getTypeValue())
    {
    case ASTTypeSpecifier::ASTInternalType::Int:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt32Ty(context_), CodeGenLLVM_Type::TypeKind::Int);
    case ASTTypeSpecifier::ASTInternalType::Int8:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt8Ty(context_), CodeGenLLVM_Type::TypeKind::Int8);
    case ASTTypeSpecifier::ASTInternalType::Int16:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt16Ty(context_), CodeGenLLVM_Type::TypeKind::Int16);
    case ASTTypeSpecifier::ASTInternalType::Int32:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt32Ty(context_), CodeGenLLVM_Type::TypeKind::Int32);
    case ASTTypeSpecifier::ASTInternalType::Int64:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt64Ty(context_), CodeGenLLVM_Type::TypeKind::Int64);
    case ASTTypeSpecifier::ASTInternalType::Int128:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt128Ty(context_), CodeGenLLVM_Type::TypeKind::Int128);
    case ASTTypeSpecifier::ASTInternalType::UInt:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt32Ty(context_), CodeGenLLVM_Type::TypeKind::UInt);
    case ASTTypeSpecifier::ASTInternalType::UInt8:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt8Ty(context_), CodeGenLLVM_Type::TypeKind::UInt8);
    case ASTTypeSpecifier::ASTInternalType::UInt16:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt16Ty(context_), CodeGenLLVM_Type::TypeKind::UInt16);
    case ASTTypeSpecifier::ASTInternalType::UInt32:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt32Ty(context_), CodeGenLLVM_Type::TypeKind::UInt32);
    case ASTTypeSpecifier::ASTInternalType::UInt64:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt64Ty(context_), CodeGenLLVM_Type::TypeKind::UInt64);
    case ASTTypeSpecifier::ASTInternalType::UInt128:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt128Ty(context_), CodeGenLLVM_Type::TypeKind::UInt128);
    case ASTTypeSpecifier::ASTInternalType::Void:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getVoidTy(context_), CodeGenLLVM_Type::TypeKind::Void);
    case ASTTypeSpecifier::ASTInternalType::Char:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt8Ty(context_), CodeGenLLVM_Type::TypeKind::Char);
    case ASTTypeSpecifier::ASTInternalType::Byte:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt8Ty(context_), CodeGenLLVM_Type::TypeKind::Byte);
    case ASTTypeSpecifier::ASTInternalType::Bool:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getInt1Ty(context_), CodeGenLLVM_Type::TypeKind::Bool);
    case ASTTypeSpecifier::ASTInternalType::Float32:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getFloatTy(context_), CodeGenLLVM_Type::TypeKind::Float32);
    case ASTTypeSpecifier::ASTInternalType::Float64:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getDoubleTy(context_), CodeGenLLVM_Type::TypeKind::Float64);
    case ASTTypeSpecifier::ASTInternalType::Float128:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getFP128Ty(context_), CodeGenLLVM_Type::TypeKind::Float128);
    case ASTTypeSpecifier::ASTInternalType::Pointer:
    {
        auto pointeeType = compileType(typeSpecifier->getInner());
        return CodeGenLLVM_Type::createPointerType(pointeeType);
    }
    case ASTTypeSpecifier::ASTInternalType::Reference:
    {
        auto pointeeType = compileType(typeSpecifier->getInner());
        return CodeGenLLVM_Type::createReferenceType(pointeeType);
    }
    case ASTTypeSpecifier::ASTInternalType::Const:
    {
        auto innerType = compileType(typeSpecifier->getInner());
        innerType->setConst();
        return innerType;
    }
    case ASTTypeSpecifier::ASTInternalType::String:
        return std::make_shared<CodeGenLLVM_Type>(llvm::PointerType::getUnqual(context_), CodeGenLLVM_Type::TypeKind::String);
    case ASTTypeSpecifier::ASTInternalType::Identifier:
    {
        // TODO: Implement identifier lookup in symbol table
        std::cerr << "Identifier lookup in type compilation not implemented yet." << std::endl;
        exit(1);
    }
    break;
    default:
        return std::make_shared<CodeGenLLVM_Type>(llvm::Type::getVoidTy(context_), CodeGenLLVM_Type::TypeKind::Void);
    }
}
