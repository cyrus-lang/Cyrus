#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"

void CodeGenLLVM_Module::compileFunctionDefinition(ASTNodePtr node)
{
    ASTFunctionDefinition *funcDef = static_cast<ASTFunctionDefinition *>(node);
    std::string functionName = static_cast<ASTIdentifier *>(funcDef->getExpr())->getName();
    ASTFunctionParameters params = funcDef->getParameters();
    ASTNodePtr body = funcDef->getBody();
    
    llvm::Type *returnType;
    if (funcDef->getReturnType().has_value())
    {   
        returnType = compileType(funcDef->getReturnType().value())->getLLVMType();
    }
    else
    {
        // returnType = llvm::Type::getVoidTy(context_);
    }

    std::cout << "Here\n";

}