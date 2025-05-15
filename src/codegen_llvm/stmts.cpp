#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include <llvm/IR/IRBuilder.h>

void CodeGenLLVM_Module::compileStmt(ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::VariableDeclaration:
        std::cerr << "variable declaration feature not implemented yet." << std::endl;
        exit(1);
        break;
    default:
        break;
    }
}