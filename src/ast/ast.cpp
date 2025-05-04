#include "ast/ast.hpp"

void printASTAccessSpecifier(ASTAccessSpecifier accessSpecifier)
{
    std::cout << "Access Specifier: ";
    switch (accessSpecifier)
    {
    case ASTAccessSpecifier::Public:
        std::cout << "Public";
        break;
    case ASTAccessSpecifier::Private:
        std::cout << "Private";
        break;
    case ASTAccessSpecifier::Abstract:
        std::cout << "Abstract";
        break;
    case ASTAccessSpecifier::Virtual:
        std::cout << "Virtual";
        break;
    case ASTAccessSpecifier::Override:
        std::cout << "Override";
        break;
    case ASTAccessSpecifier::Protected:
        std::cout << "Protected";
            break;
    case ASTAccessSpecifier::Default:
        std::cout << "Default";
        break;
    default:
        std::cout << "Unknown";
        break;
    }
    std::cout << std::endl;
}