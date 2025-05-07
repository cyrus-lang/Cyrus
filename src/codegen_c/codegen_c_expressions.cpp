#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

std::string codeGenCExpression(ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IntegerLiteral:
        std::cout << "it's IntegerLiteral" << std::endl;
        break;
    case ASTNode::NodeType::FloatLiteral:
        std::cout << "it's FloatLiteral" << std::endl;
        break;
    case ASTNode::NodeType::StringLiteral:
        std::cout << "it's StringLiteral" << std::endl;
        break;
    case ASTNode::NodeType::Identifier:
        std::cout << "it's Identifier" << std::endl;
        break;
    case ASTNode::NodeType::CastExpression:
        std::cout << "it's CastExpression" << std::endl;
        break;
    case ASTNode::NodeType::BinaryExpression:
        std::cout << "it's BinaryExpression" << std::endl;
        break;
    case ASTNode::NodeType::UnaryExpression:
        std::cout << "it's UnaryExpression" << std::endl;
        break;
    case ASTNode::NodeType::StructInitialization:
        std::cout << "it's StructInitialization" << std::endl;
        break;
    case ASTNode::NodeType::ImportedSymbolAccess:
        std::cout << "it's ImportedSymbolAccess" << std::endl;
        break;
    case ASTNode::NodeType::FunctionCall:
        std::cout << "it's FunctionCall" << std::endl;
        break;
    case ASTNode::NodeType::FieldAccess:
        std::cout << "it's FieldAccess" << std::endl;
        break;
    case ASTNode::NodeType::PointerFieldAccess:
        std::cout << "it's PointerFieldAccess" << std::endl;
        break;
    case ASTNode::NodeType::ConditionalExpression:
        std::cout << "it's ConditionalExpression" << std::endl;
        break;
    case ASTNode::NodeType::AssignmentExpression:
        std::cout << "it's AssignmentExpression" << std::endl;
        break;
    default:
        std::cerr << "Unable to generate C code for unknown expression." << std::endl;
        exit(1);
        break;
    }

    return "// expression";
}