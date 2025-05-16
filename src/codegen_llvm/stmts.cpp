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

void CodeGenLLVM_Module::compileVariableDeclaration(ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *varDecl = static_cast<ASTVariableDeclaration *>(nodePtr);

    varDecl->print(0);
}