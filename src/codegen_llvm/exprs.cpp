#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include <llvm/IR/IRBuilder.h>

llvm::Value *CodeGenLLVM_Module::compileIntegerLiteral(ASTNodePtr nodePtr)
{
    ASTIntegerLiteral *intLiteral = static_cast<ASTIntegerLiteral *>(nodePtr);
    return llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(context_),
        intLiteral->getValue());
}

llvm::Value *compileFloatLiteral(ASTNodePtr nodePtr)
{
    std::cerr << "not impl yet.";
    exit(1);
}

llvm::Value *CodeGenLLVM_Module::compileStringLiteral(ASTNodePtr nodePtr)
{
    ASTStringLiteral *stringLiteral = static_cast<ASTStringLiteral *>(nodePtr);
    llvm::Value *str = builder_.CreateGlobalStringPtr(stringLiteral->getValue());
    if (str == nullptr) {
        std::cerr << "alkdald.";
        exit(1);
    }
    return str;
}

llvm::Value *CodeGenLLVM_Module::compileBoolLiteral(ASTNodePtr nodePtr)
{
    std::cerr << "not impl yet.";
    exit(1);
}

llvm::Value *CodeGenLLVM_Module::compileFloatLiteral(ASTNodePtr nodePtr)
{
    std::cerr << "not impl yet.";
    exit(1);
}

llvm::Value *CodeGenLLVM_Module::compileExpr(ASTNodePtr nodePtr)
{   
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IntegerLiteral:
        return compileIntegerLiteral(nodePtr);
        break;
    case ASTNode::NodeType::FloatLiteral:
        return compileFloatLiteral(nodePtr);
        break;
    case ASTNode::NodeType::StringLiteral:
        return compileStringLiteral(nodePtr);
        break;
    // case ASTNode::NodeType::BoolLiteral:
    //     return compileBoolLiteral(nodePtr);
    //     break;
    default:
    {
        std::cerr << "(Error) Unknown expression type." << std::endl;
        exit(1);
    }
    break;
    }
}
