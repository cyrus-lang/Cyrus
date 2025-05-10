#include <sstream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

std::string CodeGenCGenerator::generateIfStatement(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTIfStatement *node = static_cast<ASTIfStatement *>(nodePtr);
    std::ostringstream nodeOss;

    nodeOss << "if (" << generateExpression(scope, node->getCondition()) << ") {\n";
    nodeOss <<  generateStatement(scope, node->getThenBranch());
    nodeOss << "}\n";

    if (node->getElseBranch().has_value())
    {
        nodeOss << "else {\n";
        nodeOss <<  generateStatement(scope, node->getElseBranch().value());
        nodeOss << "}";
    }

    return nodeOss.str();
}

std::string CodeGenCGenerator::generateVariable(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *node = static_cast<ASTVariableDeclaration *>(nodePtr);
    std::string type = generateTypeSpecifier(node->getTypeValue());

    std::ostringstream nodeOss;

    nodeOss << type << " " << node->getName();
    if (node->getInitializer())
    {
        nodeOss << " = ";
        nodeOss << generateExpression(scope, node->getInitializer());
    }

    nodeOss << ";" << "\n";
    scope->set(node->getName(), ScopeRecord(*node->getTypeValue()));

    return nodeOss.str();
}

std::string CodeGenCGenerator::internal_generateFunctionDeclaration(ASTNodePtr nodePtr)
{
    ASTFunctionDeclaration *node = static_cast<ASTFunctionDeclaration *>(nodePtr);
    std::stringstream nodeOss;

    if (node->getExpr() == nullptr)
    {
        std::cerr << "Unable to generate C code for function definition without an expression." << std::endl;
        exit(1);
    }
    else if (node->getExpr()->getType() != ASTNode::NodeType::Identifier)
    {
        std::cerr << "Unable to generate C code for function definition with an non-identifier expression." << std::endl;
        exit(1);
    }

    if (node->getStorageClassSpecifier().has_value())
    {
        nodeOss << generateStorageClassSpecifier(node->getStorageClassSpecifier().value());
        nodeOss << " ";
    }

    ASTTypeSpecifier *returnType = node->getReturnType();
    if (returnType == nullptr)
    {
        returnType = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Void);
    }

    nodeOss << generateTypeSpecifier(returnType) << " ";

    nodeOss << generateExpression(nullptr, node->getExpr());
    nodeOss << "(";

    std::vector<ASTFunctionParameter> parameters = node->getParameters().getList();
    for (size_t i = 0; i < parameters.size(); ++i)
    {
        ASTFunctionParameter param = parameters[i];
        ASTTypeSpecifier paramType = param.getParamType();

        nodeOss << generateTypeSpecifier(&paramType);
        nodeOss << " " << param.getParamName();

        if (i < parameters.size() - 1)
        {
            nodeOss << ", ";
        }
        else if (i == parameters.size() - 1 && node->getParameters().getIsVariadic())
        {
            nodeOss << ", ...";
        }
    }

    nodeOss << ")";
    return nodeOss.str();
}

void CodeGenCGenerator::generateFunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater)
{
    ASTFunctionDeclaration *funcDecl = static_cast<ASTFunctionDeclaration *>(nodePtr);
    headerStream_ << internal_generateFunctionDeclaration(nodePtr);
    if (!bodyLater)
    {
        headerStream_ << ";\n";
    }

    addFunction(funcDecl);
}

std::string CodeGenCGenerator::generateFunctionDefinition(ASTNodePtr nodePtr)
{
    ScopePtr scope = new Scope();
    ASTFunctionDefinition *node = static_cast<ASTFunctionDefinition *>(nodePtr);
    std::ostringstream nodeOss;

    ASTFunctionDeclaration *funcDecl = ASTFunctionDeclaration::fromFunctionDefinition(*node);
    std::string funcDeclValue = internal_generateFunctionDeclaration(funcDecl);
    nodeOss << funcDeclValue;

    switch (funcDecl->getAccessSpecifier())
    {
    case ASTAccessSpecifier::Public:
        headerStream_ << "/* PUBLIC */" << "\n";
        headerStream_ << funcDeclValue << ";";
        break;
    case ASTAccessSpecifier::Default:
    case ASTAccessSpecifier::Private:
        break;
    default:
        std::cerr << "Invalid access specifier for function. Only `public` and `private` is valid for function definitions." << std::endl;
        exit(1);
        break;
    }

    nodeOss << " {\n";
    ASTStatementList *funcBody = static_cast<ASTStatementList *>(node->getBody());
    nodeOss << generateStatementList(scope, funcBody->getStatements());
    nodeOss << "}\n";

    addFunction(funcDecl);
    return nodeOss.str();
}

std::string CodeGenCGenerator::generateReturn(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTReturnStatement *node = static_cast<ASTReturnStatement *>(nodePtr);
    std::optional<ASTNodePtr> expr = node->getExpr();
    std::ostringstream nodeOss;

    if (expr.has_value())
    {
        nodeOss << "return (";
        nodeOss << generateExpression(scope, expr.value());
        nodeOss << ");";
    }
    else
    {
        nodeOss << "return;";
    }

    nodeOss << "\n";
    return nodeOss.str();
}