#include <string>
#include <sstream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr CodeGenCGenerator::generateExpression(ScopePtr scope, ASTNodePtr nodePtr)
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
        break;
    case ASTNode::NodeType::BinaryExpression:
        return generateBinaryExpression(scope, nodePtr);
        break;
    case ASTNode::NodeType::UnaryExpression:
        return generateUnaryExpression(scope, nodePtr);
        break;
    case ASTNode::NodeType::ImportedSymbolAccess:
        // return generateImportedSymbolAccess(scope, nodePtr);
        break;
    case ASTNode::NodeType::FunctionCall:
        // return generateFunctionCall(scope, nodePtr);
        break;
    case ASTNode::NodeType::StructInitialization:
        // TODO
        break;
    case ASTNode::NodeType::FieldAccess:
        // TODO
        break;
    case ASTNode::NodeType::PointerFieldAccess:
        // TODO
        break;
    case ASTNode::NodeType::ConditionalExpression:
        // return generateConditionalExpression(scope, nodePtr);
        break;
    case ASTNode::NodeType::AssignmentExpression:
        // return generateAssignment(scope, nodePtr);
        break;
    default:
        std::cerr << "Unable to generate C code for unknown expression." << std::endl;
        exit(1);
        break;
    }
}

// REFACTOR started from here
CodeGenCValuePtr CodeGenCGenerator::generateIntegerLiteral(ASTNodePtr nodePtr)
{
    ASTIntegerLiteral *node = static_cast<ASTIntegerLiteral *>(nodePtr);
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int), std::to_string(node->getValue()), CodeGenCValue::ValueKind::Int);
}

CodeGenCValuePtr CodeGenCGenerator::generateFloatLiteral(ASTNodePtr nodePtr)
{
    ASTFloatLiteral *node = static_cast<ASTFloatLiteral *>(nodePtr);
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float32), std::to_string(node->getValue()), CodeGenCValue::ValueKind::Float);
}

CodeGenCValuePtr CodeGenCGenerator::generateStringLiteral(ASTNodePtr nodePtr)
{
    ASTStringLiteral *node = static_cast<ASTStringLiteral *>(nodePtr);
    std::string stringValue = "\"" + node->getValue() + "\"";
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::String), stringValue, CodeGenCValue::ValueKind::String);
}

CodeGenCValuePtr CodeGenCGenerator::generateIdentifier(ASTNodePtr nodePtr)
{
    ASTIdentifier *identifier = static_cast<ASTIdentifier *>(nodePtr);
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Identifier, identifier), identifier->getName(), CodeGenCValue::ValueKind::Identifier);
}

CodeGenCValuePtr CodeGenCGenerator::generateCastExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTCastExpression *castexpr = static_cast<ASTCastExpression *>(nodePtr);
    CodeGenCValuePtr exprValue = generateExpression(scope, castexpr->getExpression());
    ASTTypeSpecifier targetType = castexpr->getTargetType();
    CodeGenCValuePtr targetTypeValue = generateTypeSpecifier(&targetType);

    std::ostringstream nodeOss;
    nodeOss << "(" << targetTypeValue->getValue() << ")(" << exprValue->getValue() << ")";

    // TODO
    // Add casting rules
    // For example an array cannot be casted to a pointer
    // But an array of char can be converted to string
    // and etc.

    // TODO Check equality of data type of trueExpr and falseExpr;

    return new CodeGenCValue(targetTypeValue->getType(), nodeOss.str(), targetTypeValue->getValueKind());
}

CodeGenCValuePtr CodeGenCGenerator::generateBinaryExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTBinaryExpression *binexpr = static_cast<ASTBinaryExpression *>(nodePtr);
    CodeGenCValuePtr leftValue = generateExpression(scope, binexpr->getLeft());
    CodeGenCValuePtr rightValue = generateExpression(scope, binexpr->getRight());

    CodeGenCValue::ValueKind resultValueKind = leftValue->getValueKind();
    ASTTypeSpecifier *resultValueType = leftValue->getType();

    std::ostringstream nodeOss;

    if (leftValue->getValueKind() == CodeGenCValue::ValueKind::String || rightValue->getValueKind() == CodeGenCValue::ValueKind::String)
    {
        if (binexpr->getOperator() == ASTBinaryExpression::Operator::Add)
        {
            // 1. Determine the lengths of the strings
            std::string leftStr = leftValue->getValue();
            std::string rightStr = rightValue->getValue();
            size_t totalLength = leftStr.length() + rightStr.length();

            std::ostringstream memoryAllocOss;

            std::string resultVarName = "concatenated_string";
            nodeOss << "char* " << resultVarName << " = (char*)malloc(" << totalLength + 1 << ");\n";   
            nodeOss << memoryAllocOss.str();
            std::cout << nodeOss.str() << "\n";

            std::ostringstream strcpyOss;
            strcpyOss << "strcpy(" << resultVarName << ", " << leftValue->getValue() << ");\n";
            nodeOss << strcpyOss.str();

            std::ostringstream strcatOss;
            strcatOss << "strcat(" << resultVarName << ", " << rightValue->getValue() << ");\n";
            nodeOss << strcatOss.str();

            return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::String), nodeOss.str(), CodeGenCValue::ValueKind::String);
        }
        else
        {
            std::cerr << "Cannot build binary expression with string operands." << std::endl;
            exit(1);
        }
    }

    nodeOss << leftValue->getValue() << " ";

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

    nodeOss << " " << rightValue->getValue();
    return new CodeGenCValue(resultValueType, nodeOss.str(), resultValueKind);
}

CodeGenCValuePtr CodeGenCGenerator::generateUnaryExpression(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTUnaryExpression *unexpr = static_cast<ASTUnaryExpression *>(nodePtr);
    CodeGenCValuePtr operandValue = generateExpression(scope, unexpr->getOperand());
    CodeGenCValue::ValueKind resultValueKind = operandValue->getValueKind();
    ASTTypeSpecifier *resultValueType = operandValue->getType();

    std::ostringstream nodeOss;

    switch (unexpr->getOperator())
    {
    case ASTUnaryExpression::Operator::Plus:
        nodeOss << "+";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::Negate:
        nodeOss << "-";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::BitwiseNot:
        nodeOss << "~";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::LogicalNot:
        nodeOss << "!";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::PostIncrement:
        nodeOss << operandValue->getValue();
        nodeOss << "++";
        break;
    case ASTUnaryExpression::Operator::PostDecrement:
        nodeOss << operandValue->getValue();
        nodeOss << "--";
        break;
    case ASTUnaryExpression::Operator::PreIncrement:
        nodeOss << "++";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::PreDecrement:
        nodeOss << "--";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::AddressOf:
        nodeOss << "&";
        nodeOss << operandValue->getValue();
        break;
    case ASTUnaryExpression::Operator::Dereference:
        nodeOss << "*";
        nodeOss << operandValue->getValue();
        break;
    default:
        std::cerr << "Unable to generate C code for unknown ASTUnaryExpression::Operator." << std::endl;
        exit(1);
    }

    return new CodeGenCValue(resultValueType, nodeOss.str(), resultValueKind);
}

CodeGenCValuePtr CodeGenCGenerator::generateAssignment(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTAssignment *assignment = static_cast<ASTAssignment *>(nodePtr);
    std::ostringstream nodeOss;

    nodeOss << generateExpression(scope, assignment->getLeft());
    nodeOss << " ";
    nodeOss << assignment->getOperatorString();
    nodeOss << " ";
    nodeOss << generateExpression(scope, assignment->getRight());

    return new CodeGenCValue(nullptr, nodeOss.str(), CodeGenCValue::ValueKind::Instruction);
}

// CodeGenCValuePtr CodeGenCGenerator::generateImportedSymbolAccess(ScopePtr scope, ASTNodePtr nodePtr)
// {
//     ASTImportedSymbolAccess *symbaccess = static_cast<ASTImportedSymbolAccess *>(nodePtr);
//     std::ostringstream nodeOss;

//     if (symbaccess->getSymbolPath().size() == 1)
//     {
//         std::string recordName = symbaccess->getSymbolPath()[0];
//         std::optional<ScopeRecord *> scopeRecord = scope->get(recordName);
//         if (scopeRecord.has_value())
//         {
//             nodeOss << recordName;
//         }
//         else if (func_table_.find(recordName) != func_table_.end())
//         {
//             nodeOss << recordName;
//         }
//         else if (global_var_table_.find(recordName) != global_var_table_.end())
//         {
//             nodeOss << recordName;
//         }
//         else
//         {
//             std::cerr << "Symbol '" << recordName << "' is not defined in this scope." << std::endl;
//             exit(1);
//         }
//     }
//     else
//     {
//         // TODO Implement import module
//     }

//     return nodeOss.str();
// }

// CodeGenCValuePtr CodeGenCGenerator::generateFunctionCall(ScopePtr scope, ASTNodePtr nodePtr)
// {
//     ASTFunctionCall *funcCall = static_cast<ASTFunctionCall *>(nodePtr);
//     std::ostringstream nodeOss;

//     nodeOss << generateExpression(scope, funcCall->getExpr());
//     nodeOss << "(";

//     bool first = true;
//     for (auto &&expr : funcCall->getArguments())
//     {
//         if (!first)
//         {
//             nodeOss << ", ";
//         }
//         nodeOss << generateExpression(scope, expr);
//         first = false;
//     }

//     nodeOss << ")";

//     return nodeOss.str();
// }

// CodeGenCValuePtr CodeGenCGenerator::generateConditionalExpression(ScopePtr scope, ASTNodePtr nodePtr)
// {
//     ASTConditionalExpression *node = static_cast<ASTConditionalExpression *>(nodePtr);
//     std::ostringstream nodeOss;

//     std::string condition = generateExpression(scope, node->getCondition());
//     std::string trueExpr = generateExpression(scope, node->getTrueExpression());
//     std::string falseExpr = generateExpression(scope, node->getFalseExpression());

//     nodeOss << "(" << condition << ") ? " << trueExpr << " : " << falseExpr;

//     // TODO Check equality of data type of trueExpr and falseExpr;

//     return new CodeGenCValue(nullptr, nodeOss.str(), CodeGenCValue::ValueKind::Instruction);
// }
