#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include "codegen_llvm/scope.hpp"
#include <llvm/IR/IRBuilder.h>

void CodeGenLLVM_Module::compileStmt(OptionalScopePtr scope, ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::VariableDeclaration:
        compileVariableDeclaration(scope, nodePtr);
        break;
    default:
        break;
    }
}

void CodeGenLLVM_Module::compileStmts(OptionalScopePtr scope, ASTNodeList nodeList)
{
    for (auto &&statement : nodeList)
    {
        compileStmt(scope, statement);
    }
}