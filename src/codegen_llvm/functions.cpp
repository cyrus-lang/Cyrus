#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"

void CodeGenLLVM_Module::compileFunctionDefinition(ASTNodePtr node)
{
    // ASTFunctionDefinition *funcDef = static_cast<ASTFunctionDefinition *>(node);

    // std::string functionName = static_cast<ASTIdentifier *>(funcDef->getExpr())->getName();
    // ASTFunctionParameters params = funcDef->getParameters();
    // ASTNodePtr body = funcDef->getBody();
    // ASTTypeSpecifier *returnTypeNode = funcDef->getReturnType();

    // llvm::Type *returnType = nullptr;
    // if (returnTypeNode)
    // {
    //     returnType = compileType(returnTypeNode);
    // }
    // else
    // {
    //     returnType = llvm::Type::getVoidTy(*context_);
    // }
}