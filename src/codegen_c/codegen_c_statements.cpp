#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

std::string codeGenC_GlobalVariableDeclaration(ASTNodePtr nodePtr);
std::string codeGenC_VariableDeclaration(ASTNodePtr nodePtr);

std::pair<std::string, std::string> codeGenCStatement(ASTNodePtr statement)
{
    std::string source;
    std::string header;

    switch (statement->getType())
    {
    case ASTNode::NodeType::StatementList:
        std::cout << "it's StatementList" << std::endl;
        break;
    case ASTNode::NodeType::ImportModule:
        std::cout << "it's ImportModule" << std::endl;
        break;
    case ASTNode::NodeType::VariableDeclaration:
        // Here `NodeType::VariableDeclaration` is considered as a global variable.
        source = codeGenC_GlobalVariableDeclaration(statement) + "\n";
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
    case ASTNode::NodeType::ReturnStatement:
        std::cout << "it's ReturnStatement" << std::endl;
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
    case ASTNode::NodeType::IfStatement:
        std::cout << "it's IfStatement" << std::endl;
        break;
    case ASTNode::NodeType::TypeSpecifier:
        source = codeGenC_TypeSpecifier(statement) + "\n";
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

    return std::make_pair(source, header);
}

std::string codeGenC_GlobalVariableDeclaration(ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *node = static_cast<ASTVariableDeclaration *>(nodePtr);

    return "extern int a = 10;";
}

std::string codeGenC_VariableDeclaration(ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *node = static_cast<ASTVariableDeclaration *>(nodePtr);

    return "int a = 10;";
}
