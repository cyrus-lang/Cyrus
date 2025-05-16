#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include <llvm/IR/IRBuilder.h>

void CodeGenLLVM_Module::compileStmt(ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::VariableDeclaration:
        compileVariableDeclaration(nodePtr);
        break;
    default:
        break;
    }
}

void CodeGenLLVM_Module::compileStmts(ASTNodeList nodeList)
{
    for (auto &&statement : nodeList)
    {
        compileStmt(statement);
    }
}