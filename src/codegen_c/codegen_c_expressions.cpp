#include <string>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

#include <sstream>

std::string CodeGenCGenerator::generateExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IntegerLiteral:
        return generateIntegerLiteral(nodePtr);
    case ASTNode::NodeType::FloatLiteral:
        return generateFloatLiteral(nodePtr);
    case ASTNode::NodeType::StringLiteral:
        return generateStringLiteral(nodePtr);
    case ASTNode::NodeType::Identifier:
        return generateIdentifier(nodePtr);
    case ASTNode::NodeType::CastExpression:
        return generateCastExpression(scope, nodePtr);
    case ASTNode::NodeType::BinaryExpression:
        return generateBinaryExpression(scope, nodePtr);
    case ASTNode::NodeType::UnaryExpression:
        return generateUnaryExpression(scope, nodePtr);
    case ASTNode::NodeType::ImportedSymbolAccess:
        return generateImportedSymbolAccess(scope, nodePtr);
    case ASTNode::NodeType::FunctionCall:
        return generateFunctionCall(scope, nodePtr);
    case ASTNode::NodeType::StructInitialization:
        return "/* StructInitialization */";
    case ASTNode::NodeType::FieldAccess:
        return "/* FieldAccess */";
    case ASTNode::NodeType::PointerFieldAccess:
        return "/* PointerFieldAccess */";
    case ASTNode::NodeType::ConditionalExpression:
        return generateConditionalExpression(scope, nodePtr);
    case ASTNode::NodeType::AssignmentExpression:
        return generateAssignment(scope, nodePtr);
    default:
        std::cerr << "Unable to generate C code for unknown expression." << std::endl;
        exit(1);
        break;
    }
}

std::string CodeGenCGenerator::generateConditionalExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTConditionalExpression *node = static_cast<ASTConditionalExpression *>(nodePtr);
    std::ostringstream nodeOss;

    std::string condition = generateExpression(scope, node->getCondition());
    std::string trueExpr = generateExpression(scope, node->getTrueExpression());
    std::string falseExpr = generateExpression(scope, node->getFalseExpression());

    nodeOss << "(" << condition << ") ? " << trueExpr << " : " << falseExpr;

    return nodeOss.str();
}

std::string CodeGenCGenerator::generateIntegerLiteral(ASTNodePtr nodePtr)
{
    ASTIntegerLiteral *node = static_cast<ASTIntegerLiteral *>(nodePtr);
    return std::to_string(node->getValue());
}

std::string CodeGenCGenerator::generateFloatLiteral(ASTNodePtr nodePtr)
{
    ASTFloatLiteral *node = static_cast<ASTFloatLiteral *>(nodePtr);
    return std::to_string(node->getValue());
}

std::string CodeGenCGenerator::generateStringLiteral(ASTNodePtr nodePtr)
{
    ASTStringLiteral *node = static_cast<ASTStringLiteral *>(nodePtr);
    std::string stringValue = "\"" + node->getValue() + "\"";
    return stringValue;
}

std::string CodeGenCGenerator::generateIdentifier(ASTNodePtr nodePtr)
{
    ASTIdentifier *identifier = static_cast<ASTIdentifier *>(nodePtr);
    return identifier->getName();
}

std::string CodeGenCGenerator::generateCastExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTCastExpression *castexpr = static_cast<ASTCastExpression *>(nodePtr);
    std::string exprValue = generateExpression(scope, castexpr->getExpression());
    ASTTypeSpecifier targetType = castexpr->getTargetType();
    std::string targetTypeValue = generateTypeSpecifier(&targetType);

    std::ostringstream nodeOss;
    nodeOss << "(" << targetTypeValue << ")(" << exprValue << ")";
    return nodeOss.str();
}

std::string CodeGenCGenerator::generateBinaryExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTBinaryExpression *binexpr = static_cast<ASTBinaryExpression *>(nodePtr);
    std::string leftValue = generateExpression(scope, binexpr->getLeft());
    std::string rightValue = generateExpression(scope, binexpr->getRight());

    std::ostringstream nodeOss;

    nodeOss << leftValue << " ";

    switch (binexpr->getOperator())
    {
    case ASTBinaryExpression::Operator::Add:
        nodeOss << "+";
        break;
    case ASTBinaryExpression::Operator::Subtract:
        nodeOss << "-";
        break;
    case ASTBinaryExpression::Operator::Multiply:
        nodeOss << "*";
        break;
    case ASTBinaryExpression::Operator::Divide:
        nodeOss << "/";
        break;
    case ASTBinaryExpression::Operator::Remainder:
        nodeOss << "%";
        break;
    case ASTBinaryExpression::Operator::Equal:
        nodeOss << "==";
        break;
    case ASTBinaryExpression::Operator::NotEqual:
        nodeOss << "!=";
        break;
    case ASTBinaryExpression::Operator::LessThan:
        nodeOss << "<";
        break;
    case ASTBinaryExpression::Operator::LessEqual:
        nodeOss << "<=";
        break;
    case ASTBinaryExpression::Operator::GreaterThan:
        nodeOss << ">";
        break;
    case ASTBinaryExpression::Operator::GreaterEqual:
        nodeOss << ">=";
        break;
    case ASTBinaryExpression::Operator::LeftShift:
        nodeOss << "<<";
        break;
    case ASTBinaryExpression::Operator::RightShift:
        nodeOss << ">>";
        break;
    case ASTBinaryExpression::Operator::BitwiseAnd:
        nodeOss << "&";
        break;
    case ASTBinaryExpression::Operator::BitwiseXor:
        nodeOss << "^";
        break;
    case ASTBinaryExpression::Operator::BitwiseOr:
        nodeOss << "|";
        break;
    case ASTBinaryExpression::Operator::LogicalAnd:
        nodeOss << "&&";
        break;
    case ASTBinaryExpression::Operator::LogicalOr:
        nodeOss << "||";
        break;
    default:
        std::cerr << "Unable to generate C code for unknown ASTBinaryExpression::Operator." << std::endl;
        exit(1);
    }

    nodeOss << " " << rightValue;
    return nodeOss.str();
}

std::string CodeGenCGenerator::generateUnaryExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTUnaryExpression *unexpr = static_cast<ASTUnaryExpression *>(nodePtr);
    std::string operandValue = generateExpression(scope, unexpr->getOperand());

    std::ostringstream nodeOss;

    switch (unexpr->getOperator())
    {
    case ASTUnaryExpression::Operator::Plus:
        nodeOss << "+";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::Negate:
        nodeOss << "-";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::BitwiseNot:
        nodeOss << "~";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::LogicalNot:
        nodeOss << "!";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::PostIncrement:
        nodeOss << operandValue;
        nodeOss << "++";
        break;
    case ASTUnaryExpression::Operator::PostDecrement:
        nodeOss << operandValue;
        nodeOss << "--";
        break;
    case ASTUnaryExpression::Operator::PreIncrement:
        nodeOss << "++";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::PreDecrement:
        nodeOss << "--";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::AddressOf:
        nodeOss << "&";
        nodeOss << operandValue;
        break;
    case ASTUnaryExpression::Operator::Dereference:
        nodeOss << "*";
        nodeOss << operandValue;
        break;
    default:
        std::cerr << "Unable to generate C code for unknown ASTUnaryExpression::Operator." << std::endl;
        exit(1);
    }

    return nodeOss.str();
}

std::string CodeGenCGenerator::generateImportedSymbolAccess(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTImportedSymbolAccess *symbaccess = static_cast<ASTImportedSymbolAccess *>(nodePtr);
    std::ostringstream nodeOss;

    if (symbaccess->getSymbolPath().size() == 1)
    {
        std::string recordName = symbaccess->getSymbolPath()[0];
        std::optional<ScopeRecord *> scopeRecord = scope->get(recordName);
        if (scopeRecord.has_value())
        {
            nodeOss << recordName;
        }
        else if (func_table_.find(recordName) != func_table_.end())
        {
            nodeOss << recordName;
        }
        else if (global_var_table_.find(recordName) != global_var_table_.end())
        {
            nodeOss << recordName;
        }
        else
        {
            std::cerr << "Symbol '" << recordName << "' is not defined in this scope." << std::endl;
            exit(1);
        }
    }
    else
    {
        // TODO Implement import module
    }

    return nodeOss.str();
}

std::string CodeGenCGenerator::generateFunctionCall(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTFunctionCall *funcCall = static_cast<ASTFunctionCall *>(nodePtr);
    std::ostringstream nodeOss;

    nodeOss << generateExpression(scope, funcCall->getExpr());
    nodeOss << "(";

    bool first = true;
    for (auto &&expr : funcCall->getArguments())
    {
        if (!first)
        {
            nodeOss << ", ";
        }
        nodeOss << generateExpression(scope, expr);
        first = false;
    }

    nodeOss << ")";

    return nodeOss.str();
}

std::string CodeGenCGenerator::generateAssignment(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTAssignment *assignment = static_cast<ASTAssignment *>(nodePtr);
    std::ostringstream nodeOss;

    nodeOss << generateExpression(scope, assignment->getLeft());
    nodeOss << " ";
    nodeOss << assignment->getOperatorString();
    nodeOss << " ";
    nodeOss << generateExpression(scope, assignment->getRight());

    return nodeOss.str();
}