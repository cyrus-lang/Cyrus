#include "ast/ast.hpp"
#include "codegen_llvm/values.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/scope.hpp"
#include <llvm/IR/IRBuilder.h>

CodeGenLLVM_EValue CodeGenLLVM_Module::compileIntegerLiteral(ASTNodePtr nodePtr)
{
    ASTIntegerLiteral *intLiteral = static_cast<ASTIntegerLiteral *>(nodePtr);
    std::unique_ptr<ASTTypeSpecifier> astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::Int);
    CodeGenLLVM_Type *type = compileType(astType.get());
    auto value = llvm::ConstantInt::get(type->getLLVMType(), intLiteral->getValue());
    return CodeGenLLVM_EValue(new CodeGenLLVM_Value(value, type), CodeGenLLVM_EValue::ValueCategory::RValue);
}

CodeGenLLVM_EValue CodeGenLLVM_Module::compileFloatLiteral(ASTNodePtr nodePtr)
{
    ASTFloatLiteral *floatLiteral = static_cast<ASTFloatLiteral *>(nodePtr);
    std::unique_ptr<ASTTypeSpecifier> astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::Float32);
    CodeGenLLVM_Type *type = compileType(astType.get());
    auto value = llvm::ConstantFP::get(type->getLLVMType(), floatLiteral->getValue());
    return CodeGenLLVM_EValue(new CodeGenLLVM_Value(value, type), CodeGenLLVM_EValue::ValueCategory::RValue);
}

CodeGenLLVM_EValue CodeGenLLVM_Module::compileStringLiteral(ASTNodePtr nodePtr)
{
    ASTStringLiteral *stringLiteral = static_cast<ASTStringLiteral *>(nodePtr);
    std::unique_ptr<ASTTypeSpecifier> astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::String);
    CodeGenLLVM_Type *type = compileType(astType.get());
    llvm::Value *value = builder_.CreateGlobalStringPtr(stringLiteral->getValue());
    return CodeGenLLVM_EValue(new CodeGenLLVM_Value(value, type), CodeGenLLVM_EValue::ValueCategory::RValue);
}

CodeGenLLVM_EValue CodeGenLLVM_Module::compileBoolLiteral(ASTNodePtr nodePtr)
{
    ASTBoolLiteral *boolLiteral = static_cast<ASTBoolLiteral *>(nodePtr);
    std::unique_ptr<ASTTypeSpecifier> astType = std::make_unique<ASTTypeSpecifier>(ASTTypeSpecifier::ASTInternalType::Bool);
    CodeGenLLVM_Type *type = compileType(astType.get());
    int boolValue = boolLiteral->getValue() ? 1 : 0;
    auto value = llvm::ConstantInt::get(type->getLLVMType(), boolValue);
    return CodeGenLLVM_EValue(new CodeGenLLVM_Value(value, type), CodeGenLLVM_EValue::ValueCategory::RValue);
}

CodeGenLLVM_EValue CodeGenLLVM_Module::compileExpr(OptionalScopePtr scope, ASTNodePtr nodePtr)
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
    {
        std::cerr << "(Error) Unknown expression type." << std::endl;
        exit(1);
    }
    break;
    }
}
