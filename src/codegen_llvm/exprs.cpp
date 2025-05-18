#include "ast/ast.hpp"
#include "codegen_llvm/values.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/scope.hpp"
#include <llvm/IR/IRBuilder.h>
#include <memory>

std::shared_ptr<CodeGenLLVM_EValue> CodeGenLLVM_Module::compileIntegerLiteral(ASTNodePtr nodePtr)
{
    auto intLiteral = static_cast<ASTIntegerLiteral *>(nodePtr);
    auto astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::Int);
    auto type = compileType(astType.get());
    auto value = llvm::ConstantInt::get(type->getLLVMType(), intLiteral->getValue());
    auto valPtr = std::make_shared<CodeGenLLVM_Value>(value, type);
    return std::make_shared<CodeGenLLVM_EValue>(valPtr, CodeGenLLVM_EValue::ValueCategory::RValue);
}

std::shared_ptr<CodeGenLLVM_EValue> CodeGenLLVM_Module::compileFloatLiteral(ASTNodePtr nodePtr)
{
    auto floatLiteral = static_cast<ASTFloatLiteral *>(nodePtr);
    auto astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::Float32);
    auto type = compileType(astType.get());
    auto value = llvm::ConstantFP::get(type->getLLVMType(), floatLiteral->getValue());
    auto valPtr = std::make_shared<CodeGenLLVM_Value>(value, type);
    return std::make_shared<CodeGenLLVM_EValue>(valPtr, CodeGenLLVM_EValue::ValueCategory::RValue);
}

std::shared_ptr<CodeGenLLVM_EValue> CodeGenLLVM_Module::compileStringLiteral(ASTNodePtr nodePtr)
{
    auto stringLiteral = static_cast<ASTStringLiteral *>(nodePtr);
    auto astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::String);
    auto type = compileType(astType.get());
    auto value = builder_.CreateGlobalStringPtr(stringLiteral->getValue());
    auto valPtr = std::make_shared<CodeGenLLVM_Value>(value, type);
    return std::make_shared<CodeGenLLVM_EValue>(valPtr, CodeGenLLVM_EValue::ValueCategory::RValue);
}

std::shared_ptr<CodeGenLLVM_EValue> CodeGenLLVM_Module::compileBoolLiteral(ASTNodePtr nodePtr)
{
    auto boolLiteral = static_cast<ASTBoolLiteral *>(nodePtr);
    auto astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::Bool);
    auto type = compileType(astType.get());
    int boolValue = boolLiteral->getValue() ? 1 : 0;
    auto value = llvm::ConstantInt::get(type->getLLVMType(), boolValue);
    auto valPtr = std::make_shared<CodeGenLLVM_Value>(value, type);
    return std::make_shared<CodeGenLLVM_EValue>(valPtr, CodeGenLLVM_EValue::ValueCategory::RValue);
}

std::shared_ptr<CodeGenLLVM_EValue> CodeGenLLVM_Module::compileExpr(OptionalScopePtr scope, ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IntegerLiteral:
        return compileIntegerLiteral(nodePtr);
    case ASTNode::NodeType::FloatLiteral:
        return compileFloatLiteral(nodePtr);
    case ASTNode::NodeType::StringLiteral:
        return compileStringLiteral(nodePtr);
    case ASTNode::NodeType::BoolLiteral:
        return compileBoolLiteral(nodePtr);
    default:
        std::cerr << "(Error) Unknown expression type." << std::endl;
        exit(1);
    }
}