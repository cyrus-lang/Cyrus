#include <iostream>
#include <sstream>
#include "codegen_c/codegen_c.hpp"

void CodeGenC::generate()
{
    // std::cout << opts_.getOutputDirectory().value() << std::endl;
}

void CodeGenC::compile()
{
}

std::string codeGenC_GlobalVariableDeclaration(ASTNodePtr nodePtr) {
    ASTVariableDeclaration* node = static_cast<ASTVariableDeclaration*>(nodePtr);

    return "int a = 10;";
}

std::string codeGenC_VariableDeclaration(ASTNodePtr nodePtr) {
    ASTVariableDeclaration* node = static_cast<ASTVariableDeclaration*>(nodePtr);

    return "extern int a = 10;";
}

std::pair<std::string, std::string> CodeGenCSourceFile::generate()
{   
    std::stringstream sourceStream;
    std::stringstream headerStream;

    for (auto &&statement : program_->getStatements())
    {
        switch (statement->getType())
        {
        case ASTNode::NodeType::StatementList:
            std::cout << "it's StatementList" << std::endl;
            break;
        case ASTNode::NodeType::ImportModule:
            std::cout << "it's ImportModule" << std::endl;
            break;
        case ASTNode::NodeType::VariableDeclaration:
            sourceStream << codeGenC_GlobalVariableDeclaration(statement) << std::endl;
            break;
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
        case ASTNode::NodeType::TypeSpecifier:
            std::cout << "it's TypeSpecifier" << std::endl;
            break;
        case ASTNode::NodeType::ImportStatement:
            std::cout << "it's ImportStatement" << std::endl;
            break;
        case ASTNode::NodeType::FunctionDefinition:
            std::cout << "it's FunctionDefinition" << std::endl;
            break;
        case ASTNode::NodeType::FunctionParameter:
            std::cout << "it's FunctionParameter" << std::endl;
            break;
        case ASTNode::NodeType::ASTVariableDeclaration:
            std::cout << "it's ASTVariableDeclaration" << std::endl;
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
        case ASTNode::NodeType::StructInitialization:
            std::cout << "it's StructInitialization" << std::endl;
            break;
        case ASTNode::NodeType::ConditionalExpression:
            std::cout << "it's ConditionalExpression" << std::endl;
            break;
        case ASTNode::NodeType::AssignmentExpression:
            std::cout << "it's AssignmentExpression" << std::endl;
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
        case ASTNode::NodeType::EnumVariant:
            std::cout << "it's EnumVariant" << std::endl;
            break;
        case ASTNode::NodeType::EnumDefinition:
            std::cout << "it's EnumDefinition" << std::endl;
            break;
        case ASTNode::NodeType::ReturnStatement:
            std::cout << "it's ReturnStatement" << std::endl;
            break;
        case ASTNode::NodeType::ContinueStatement:
            std::cout << "it's ContinueStatement" << std::endl;
            break;
        case ASTNode::NodeType::BreakStatement:
            std::cout << "it's BreakStatement" << std::endl;
            break;
        case ASTNode::NodeType::ForStatement:
            std::cout << "it's ForStatement" << std::endl;
            break;
        case ASTNode::NodeType::IfStatement:
            std::cout << "it's IfStatement" << std::endl;
            break;
        case ASTNode::NodeType::Program:
            std::cerr << "Unable to generate C code for top level node ASTProgram." << std::endl;
            exit(1);
            break;
        default:
            std::cerr << "Unable to generate C code for unknown ASTNode." << std::endl;
            exit(1);
            break;
        }
    }

    return std::make_pair(sourceStream.str(), headerStream.str());
}

