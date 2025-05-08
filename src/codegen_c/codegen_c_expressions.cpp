#include <string>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr codeGenC_IntegerLiteral(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_FloatLiteral(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_StringLiteral(ASTNodePtr nodePtr);

CodeGenCValuePtr codeGenCExpression(ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IntegerLiteral:
        return codeGenC_IntegerLiteral(nodePtr);
        break;
    case ASTNode::NodeType::FloatLiteral:
        return codeGenC_FloatLiteral(nodePtr);
        break;
    case ASTNode::NodeType::StringLiteral:
        return codeGenC_StringLiteral(nodePtr);
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
}

CodeGenCValuePtr codeGenC_IntegerLiteral(ASTNodePtr nodePtr)
{
    ASTIntegerLiteral *node = static_cast<ASTIntegerLiteral *>(nodePtr);
    return new CodeGenCValue(std::to_string(node->getValue()), std::string(), CodeGenCValue::ValueType::RValue);
}

CodeGenCValuePtr codeGenC_FloatLiteral(ASTNodePtr nodePtr)
{
    ASTFloatLiteral *node = static_cast<ASTFloatLiteral *>(nodePtr);
    return new CodeGenCValue(std::to_string(node->getValue()), std::string(), CodeGenCValue::ValueType::RValue);
}

CodeGenCValuePtr codeGenC_StringLiteral(ASTNodePtr nodePtr)
{
    ASTStringLiteral *node = static_cast<ASTStringLiteral *>(nodePtr);
    std::string stringValue = "\"" + node->getValue() + "\"";
    return new CodeGenCValue(stringValue, std::string(), CodeGenCValue::ValueType::RValue);
}