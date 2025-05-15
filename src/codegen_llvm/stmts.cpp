#include "ast/ast.hpp"
#include "codegen_llvm/compiler.hpp"
#include <llvm/IR/IRBuilder.h>

llvm::Value *CodeGenLLVM_Module::compileStmt(ASTNodePtr nodePtr)
{
    std::cerr << "Feature not implemented yet." << std::endl;
    exit(1);

    return nullptr;
}