#include "ast/ast.hpp"

const std::string formatAccessSpecifier(ASTAccessSpecifier accessSpecifier)
{
    switch (accessSpecifier)
    {
    case ASTAccessSpecifier::Public:
        return "public";
        break;
    case ASTAccessSpecifier::Private:
        return "private";
        break;
    case ASTAccessSpecifier::Abstract:
        return "abstract";
        break;
    case ASTAccessSpecifier::Virtual:
        return "virtual";
        break;
    case ASTAccessSpecifier::Override:
        return "override";
        break;
    case ASTAccessSpecifier::Protected:
        return "protected";
        break;
    case ASTAccessSpecifier::Default:
        return "default";
        break;
    default:
        return "unknown";
        break;
    }
}

void printAccessSpecifier(ASTAccessSpecifier accessSpecifier)
{
    std::cout << "Access Specifier: ";
    std::cout << formatAccessSpecifier(accessSpecifier);
    std::cout << std::endl;
}
