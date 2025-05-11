#include "ast/ast.hpp"
#include "codegen_c/codegen_c_scope.hpp"
#include "codegen_c/codegen_c_generator.hpp"
#include <iostream>

std::pair<std::string, std::string> CodeGenCSourceFile::generate()
{
    CodeGenCGenerator generator = CodeGenCGenerator(*this);
    return std::make_pair(generator.getSourceStream().str(), generator.getHeaderStream().str());
}

void CodeGenCGenerator::generateTopLevel(ASTNodeList nodeList)
{
    for (auto &&statement : nodeList)
    {
        switch (statement->getType())
        {
        case ASTNode::NodeType::ImportStatement:
            std::cerr << "Import statement not implemented yet." << std::endl;
            exit(1);
            break;
        case ASTNode::NodeType::VariableDeclaration:
            sourceStream_ << generateVariable(nullptr, statement)->getValue();
            break;
        case ASTNode::NodeType::FunctionDefinition:
            sourceStream_ << generateFunctionDefinition(statement)->getValue();
            break;
        case ASTNode::NodeType::FunctionDeclaration:
            generateFunctionDeclaration(statement, false);
            break;
        default:
            sourceStream_ << generateStatement(nullptr, statement)->getValue();
            break;
        }
    }

    sourceStream_ << "\n";
}

CodeGenCValuePtr CodeGenCGenerator::generateStatementList(ScopePtr scope, ASTNodeList nodeList)
{
    std::ostringstream nodeOss;

    for (auto &&statement : nodeList)
    {
        nodeOss << generateStatement(scope, statement)->getValue();
    }

    return new CodeGenCValue(nullptr, nodeOss.str(), CodeGenCValue::ValueKind::Instruction);
}

CodeGenCValuePtr CodeGenCGenerator::generateStatementList(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTStatementList *statementList = static_cast<ASTStatementList *>(nodePtr);
    const ASTNodeList &nodeList = statementList->getStatements();
    std::ostringstream nodeOss;

    for (auto &&statement : nodeList)
    {
        nodeOss << generateStatement(scope, statement)->getValue();
    }

    return new CodeGenCValue(nullptr, nodeOss.str(), CodeGenCValue::ValueKind::Instruction);
}

CodeGenCValuePtr CodeGenCGenerator::generateStatement(ScopePtr scope, ASTNodePtr nodePtr)
{
    switch (nodePtr->getType())
    {
    case ASTNode::NodeType::IfStatement:
        return generateIfStatement(scope, nodePtr);
        break;
    case ASTNode::NodeType::VariableDeclaration:
        return generateVariable(scope, nodePtr);
        break;
    case ASTNode::NodeType::ReturnStatement:
        return generateReturn(scope, nodePtr);
        break;
    case ASTNode::NodeType::TypeDefStatement:
        std::cout << "it's TypeDefStatement" << std::endl;
        break;
    case ASTNode::NodeType::StructDefinition:
        std::cout << "it's StructDefinition" << std::endl;
        break;
    case ASTNode::NodeType::StructField:
        std::cout << "it's StructField" << std::endl;
        break;
    case ASTNode::NodeType::EnumVariant:
        std::cout << "it's EnumVariant" << std::endl;
        break;
    case ASTNode::NodeType::EnumDefinition:
        std::cout << "it's EnumDefinition" << std::endl;
        break;
    case ASTNode::NodeType::ForStatement:
        std::cout << "it's ForStatement" << std::endl;
        break;
    case ASTNode::NodeType::ContinueStatement:
        std::cout << "it's ContinueStatement" << std::endl;
        break;
    case ASTNode::NodeType::BreakStatement:
        std::cout << "it's BreakStatement" << std::endl;
        break;
    case ASTNode::NodeType::StatementList:
        return generateStatementList(scope, nodePtr);
        break;
    case ASTNode::NodeType::ImportStatement:
        std::cerr << "Cannot build import inside an another statement. It must be defined at the top level of your program." << std::endl;
        exit(1);
    case ASTNode::NodeType::Program:
        std::cerr << "Unable to generate C code for top level node ASTProgram." << std::endl;
        exit(1);
        break;
    case ASTNode::NodeType::ImportedSymbolAccess:
        // ignore unused loadings
        break;
    default:
    {
        CodeGenCValuePtr exprValue = generateExpression(scope, nodePtr);
        exprValue->getValue() += std::string(";\n");
        return exprValue;
    }
    }
}
