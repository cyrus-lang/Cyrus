#include <sstream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

void CodeGenCGenerator::generateVariable(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *node = static_cast<ASTVariableDeclaration *>(nodePtr);
    std::string type = generateTypeSpecifier(node->getTypeValue());

    sourceStream_ << type << " " << node->getName();
    if (node->getInitializer())
    {
        sourceStream_ << " = ";
        generateExpression(scope, node->getInitializer());
    }

    sourceStream_ << ";" << "\n";
    scope->set(node->getName(), ScopeRecord(*node->getTypeValue()));
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

    std::vector<ASTFunctionParameter> parameters = node->getParameters();
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
    }

    nodeOss << ")";
    return nodeOss.str();
}

void CodeGenCGenerator::generateFunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater)
{
    headerStream_ << internal_generateFunctionDeclaration(nodePtr);
    if (!bodyLater)
    {
        headerStream_ << ";\n";
    }
}

void CodeGenCGenerator::generateFunctionDefinition(ASTNodePtr nodePtr)
{
    ScopePtr scope = new Scope();
    ASTFunctionDefinition *node = static_cast<ASTFunctionDefinition *>(nodePtr);

    ASTFunctionDeclaration *funcDecl = ASTFunctionDeclaration::fromFunctionDefinition(*node);
    std::string funcDeclValue = internal_generateFunctionDeclaration(funcDecl);
    sourceStream_ << funcDeclValue;

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

    sourceStream_ << " {\n";
    ASTStatementList *funcBody = static_cast<ASTStatementList *>(node->getBody());
    generateStatementList(scope, funcBody->getStatements());
    sourceStream_ << "}\n";
}

void CodeGenCGenerator::generateReturn(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTReturnStatement *node = static_cast<ASTReturnStatement *>(nodePtr);
    std::optional<ASTNodePtr> expr = node->getExpr();

    if (expr.has_value())
    {
        sourceStream_ << "return (";
        sourceStream_ << generateExpression(scope, expr.value());
        sourceStream_ << ");";
    }
    else
    {
        sourceStream_ << "return;";
    }

    sourceStream_ << "\n";
}