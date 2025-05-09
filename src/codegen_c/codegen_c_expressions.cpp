#include <string>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr codeGenC_BinaryExpression(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_UnaryExpression(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_CastExpression(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_IntegerLiteral(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_StringLiteral(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_FloatLiteral(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_Identifier(ASTNodePtr nodePtr);

CodeGenCValuePtr codeGenCExpression(ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IntegerLiteral:
        return codeGenC_IntegerLiteral(nodePtr);
    case ASTNode::NodeType::FloatLiteral:
        return codeGenC_FloatLiteral(nodePtr);
    case ASTNode::NodeType::StringLiteral:
        return codeGenC_StringLiteral(nodePtr);
    case ASTNode::NodeType::Identifier:
        return codeGenC_Identifier(nodePtr);
    case ASTNode::NodeType::CastExpression:
        return codeGenC_CastExpression(nodePtr);
        break;
    case ASTNode::NodeType::BinaryExpression:
        return codeGenC_BinaryExpression(nodePtr);
    case ASTNode::NodeType::UnaryExpression:
        return codeGenC_UnaryExpression(nodePtr);
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

CodeGenCValuePtr codeGenC_Identifier(ASTNodePtr nodePtr)
{
    ASTIdentifier *identifier = static_cast<ASTIdentifier *>(nodePtr);
    return new CodeGenCValue(identifier->getName(), std::string(), CodeGenCValue::ValueType::LValue);
}

CodeGenCValuePtr codeGenC_BinaryExpression(ASTNodePtr nodePtr)
{
    ASTBinaryExpression *binexpr = static_cast<ASTBinaryExpression *>(nodePtr);
    CodeGenCValuePtr leftValue = codeGenCExpression(binexpr->getLeft());
    CodeGenCValuePtr rightValue = codeGenCExpression(binexpr->getRight());

    std::ostringstream sourceOss;
    std::ostringstream headerOss;
    headerOss << leftValue->getHeader();
    headerOss << rightValue->getHeader();

    sourceOss << leftValue->getSource() << " ";

    switch (binexpr->getOperator())
    {
    case ASTBinaryExpression::Operator::Add:
        sourceOss << "+";
        break;
    case ASTBinaryExpression::Operator::Subtract:
        sourceOss << "-";
        break;
    case ASTBinaryExpression::Operator::Multiply:
        sourceOss << "*";
        break;
    case ASTBinaryExpression::Operator::Divide:
        sourceOss << "/";
        break;
    case ASTBinaryExpression::Operator::Remainder:
        sourceOss << "%";
        break;
    case ASTBinaryExpression::Operator::Equal:
        sourceOss << "==";
        break;
    case ASTBinaryExpression::Operator::NotEqual:
        sourceOss << "!=";
        break;
    case ASTBinaryExpression::Operator::LessThan:
        sourceOss << "<";
        break;
    case ASTBinaryExpression::Operator::LessEqual:
        sourceOss << "<=";
        break;
    case ASTBinaryExpression::Operator::GreaterThan:
        sourceOss << ">";
        break;
    case ASTBinaryExpression::Operator::GreaterEqual:
        sourceOss << ">=";
        break;
    case ASTBinaryExpression::Operator::LeftShift:
        sourceOss << "<<";
        break;
    case ASTBinaryExpression::Operator::RightShift:
        sourceOss << ">>";
        break;
    case ASTBinaryExpression::Operator::BitwiseAnd:
        sourceOss << "&";
        break;
    case ASTBinaryExpression::Operator::BitwiseXor:
        sourceOss << "^";
        break;
    case ASTBinaryExpression::Operator::BitwiseOr:
        sourceOss << "|";
        break;
    case ASTBinaryExpression::Operator::LogicalAnd:
        sourceOss << "&&";
        break;
    case ASTBinaryExpression::Operator::LogicalOr:
        sourceOss << "||";
        break;
    default:
        std::cerr << "Unable to generate C code for unknown ASTBinaryExpression::Operator." << std::endl;
        exit(1);
    }

    sourceOss << " " << rightValue->getSource();
    return new CodeGenCValue(sourceOss.str(), headerOss.str(), CodeGenCValue::ValueType::LValue);
}

CodeGenCValuePtr codeGenC_UnaryExpression(ASTNodePtr nodePtr)
{
    ASTUnaryExpression *unexpr = static_cast<ASTUnaryExpression *>(nodePtr);
    CodeGenCValuePtr operandValue = codeGenCExpression(unexpr->getOperand());

    std::ostringstream sourceOss;
    std::ostringstream headerOss;

    headerOss << operandValue->getHeader();

    switch (unexpr->getOperator())
    {
    case ASTUnaryExpression::Operator::Plus:
        sourceOss << "+";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::Negate:
        sourceOss << "-";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::BitwiseNot:
        sourceOss << "~";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::LogicalNot:
        sourceOss << "!";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::PostIncrement:
        sourceOss << operandValue->getSource();
        sourceOss << "++";
        break;
    case ASTUnaryExpression::Operator::PostDecrement:
        sourceOss << operandValue->getSource();
        sourceOss << "--";
        break;
    case ASTUnaryExpression::Operator::PreIncrement:
        sourceOss << "++";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::PreDecrement:
        sourceOss << "--";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::AddressOf:
        sourceOss << "&";
        sourceOss << operandValue->getSource();
        break;
    case ASTUnaryExpression::Operator::Dereference:
        sourceOss << "*";
        sourceOss << operandValue->getSource();
        break;
    default:
        std::cerr << "Unable to generate C code for unknown ASTUnaryExpression::Operator." << std::endl;
        exit(1);
    }

    return new CodeGenCValue(sourceOss.str(), headerOss.str(), CodeGenCValue::ValueType::LValue);
}

CodeGenCValuePtr codeGenC_CastExpression(ASTNodePtr nodePtr)
{
    ASTCastExpression *castexpr = static_cast<ASTCastExpression *>(nodePtr);
    CodeGenCValuePtr exprValue = codeGenCExpression(castexpr->getExpression());
    ASTTypeSpecifier targetType = castexpr->getTargetType();
    CodeGenCValuePtr targetTypeValue = codeGenC_TypeSpecifier(&targetType);

    std::ostringstream sourceOss;
    std::ostringstream headerOss;

    headerOss << exprValue->getHeader();
    headerOss << targetTypeValue->getHeader();

    sourceOss << "(" << targetTypeValue->getSource() << ")(" << exprValue->getSource() << ")";

    return new CodeGenCValue(sourceOss.str(), headerOss.str(), CodeGenCValue::ValueType::LValue);
}