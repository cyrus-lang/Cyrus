#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include <llvm/IR/IRBuilder.h>

llvm::Value *CodeGenLLVM_Module::compileIntegerLiteral(ASTNodePtr node)
{
    ASTIntegerLiteral *intLiteral = static_cast<ASTIntegerLiteral *>(node);
    return llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(context_),
        intLiteral->getValue());
}

llvm::Value *compileFloatLiteral(ASTNodePtr node)
{
    std::cerr << "not impl yet.";
    exit(1);
}

llvm::Value *CodeGenLLVM_Module::compileStringLiteral(ASTNodePtr node)
{
    ASTStringLiteral *stringLiteral = static_cast<ASTStringLiteral *>(node);
    llvm::Value *str = builder_.CreateGlobalStringPtr(stringLiteral->getValue());
}

llvm::Value *CodeGenLLVM_Module::compileBoolLiteral(ASTNodePtr node)
{
    std::cerr << "not impl yet.";
    exit(1);
}

llvm::Value *CodeGenLLVM_Module::compileFloatLiteral(ASTNodePtr node)
{
    std::cerr << "not impl yet.";
    exit(1);
}

llvm::Value *CodeGenLLVM_Module::compileLiteral(ASTNodePtr node)
{
    if (node->getType() == ASTNode::NodeType::IntegerLiteral)
    {
        return compileIntegerLiteral(node);
    }
    else if (node->getType() == ASTNode::NodeType::FloatLiteral)
    {
        return compileFloatLiteral(node);
    }
    else if (node->getType() == ASTNode::NodeType::StringLiteral)
    {
        return compileStringLiteral(node);
    }
    // else if (node->getType() == ASTNode::NodeType::BooleanLiteral)
    // {
    //     return compileBoolLiteral(node);
    // }
    else
    {
        std::cerr << "(Error) Unknown literal." << std::endl;
        exit(1);
    }
}

llvm::Value *CodeGenLLVM_Module::compileExpr(ASTNodePtr node)
{
    std::cerr << "not impl yet.";
    exit(1);
}
