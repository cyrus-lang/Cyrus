#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

std::string codeGenC_TypeSpecifier(ASTNodePtr nodePtr)
{
    ASTTypeSpecifier *node = static_cast<ASTTypeSpecifier *>(nodePtr);
    switch (node->getTypeValue())
    {
    case ASTTypeSpecifier::ASTInternalType::Int:
        return "int";
        break;
    default:
        std::cerr << "Unable to generate C code for unsupported ASTTypeSpecifier::ASTInternalType." << std::endl;
        exit(1);
        break;
    };
}