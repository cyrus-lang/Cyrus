#include <sstream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr codeGenC_ReturnStatement(ScopePtr scope, ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_FunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater = false);
CodeGenCValuePtr codeGenC_FunctionDefinition(ASTNodePtr nodePtr);

std::pair<std::string, std::string> codeGenCStatement(ScopePtr scope, ASTNodePtr statement)
{
    CodeGenCValuePtr value = nullptr;

    switch (statement->getType())
    {
    case ASTNode::NodeType::VariableDeclaration:
        value = codeGenC_VariableDeclaration(scope, statement);
        break;
    case ASTNode::NodeType::ReturnStatement:
        value = codeGenC_ReturnStatement(scope, statement);
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
        value = codeGenC_TypeSpecifier(statement);
        break;
    case ASTNode::NodeType::StatementList:
        value = codeGenCStatementList(scope, statement);
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
        return std::make_pair("", "");
    default:    
        auto temp = codeGenCExpression(scope, statement);
        return std::make_pair(temp->getSource() + ";", temp->getHeader());
    }

    return std::make_pair(value->getSource(), value->getHeader());
}

CodeGenCValuePtr codeGenC_VariableDeclaration(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *node = static_cast<ASTVariableDeclaration *>(nodePtr);
    CodeGenCValuePtr typeValue = codeGenC_TypeSpecifier(node->getTypeValue());

    std::ostringstream oss;

    oss << typeValue->getSource() << " " << node->getName();
    if (node->getInitializer())
    {
        CodeGenCValuePtr exprValue = codeGenCExpression(scope, node->getInitializer());
        oss << " = " << exprValue->getSource();
        delete exprValue;
    }

    oss << ";" << "\n";

    scope->set(node->getName(), ScopeRecord(*node->getTypeValue()));

    delete typeValue;
    return new CodeGenCValue(oss.str(), std::string(), CodeGenCValue::ValueType::Instruction);
}

CodeGenCValuePtr codeGenC_FunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater)
{
    ASTFunctionDeclaration *node = static_cast<ASTFunctionDeclaration *>(nodePtr);

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

    CodeGenCValuePtr exprValue = codeGenCExpression(nullptr, node->getExpr());
    std::ostringstream funcDeclOss;

    if (node->getStorageClassSpecifier().has_value())
    {
        CodeGenCValuePtr storageClassSpecifier = codeGenC_StorageClassSpecifier(node->getStorageClassSpecifier().value());
        funcDeclOss << storageClassSpecifier->getSource() << " ";
    }

    ASTTypeSpecifier *returnType = node->getReturnType();
    if (returnType == nullptr)
    {
        returnType = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Void);
    }

    CodeGenCValuePtr returnTypeValue = codeGenC_TypeSpecifier(returnType);

    funcDeclOss << returnTypeValue->getSource() << " " << exprValue->getSource() << "(";

    std::vector<ASTFunctionParameter> parameters = node->getParameters();
    for (size_t i = 0; i < parameters.size(); ++i)
    {
        ASTFunctionParameter param = parameters[i];

        ASTTypeSpecifier paramType = param.getParamType();
        CodeGenCValuePtr paramTypeValue = codeGenC_TypeSpecifier(&paramType);
        funcDeclOss << paramTypeValue->getSource();

        funcDeclOss << " " << param.getParamName();

        if (i < parameters.size() - 1)
        {
            funcDeclOss << ", ";
        }

        delete paramTypeValue;
    }
    funcDeclOss << ")";

    if (!bodyLater)
    {
        funcDeclOss << ";\n";
    }

    return new CodeGenCValue(std::string(), funcDeclOss.str(), CodeGenCValue::ValueType::Instruction);
}

CodeGenCValuePtr codeGenC_FunctionDefinition(ASTNodePtr nodePtr)
{
    ScopePtr scope = new Scope();
    ASTFunctionDefinition *node = static_cast<ASTFunctionDefinition *>(nodePtr);

    std::ostringstream funcDeclOss;
    ASTFunctionDeclaration *funcDecl = ASTFunctionDeclaration::fromFunctionDefinition(*node);
    CodeGenCValuePtr funcDeclValue = codeGenC_FunctionDeclaration(funcDecl, true);
    funcDeclOss << funcDeclValue->getHeader() << ";\n";

    std::ostringstream funcDefOss;
    funcDefOss << funcDeclValue->getHeader() << " {\n";

    ASTStatementList *funcBody = static_cast<ASTStatementList *>(node->getBody());
    auto [bodySource, bodyHeaders] = codeGenCStatementList(scope, funcBody->getStatements());
    funcDefOss << bodySource;

    funcDefOss << "}\n";
    funcDeclOss << bodyHeaders;

    return new CodeGenCValue(funcDefOss.str(), funcDeclOss.str(), CodeGenCValue::ValueType::Instruction);
}

CodeGenCValuePtr codeGenC_ReturnStatement(ScopePtr scope, ASTNodePtr nodePtr)
{
    ASTReturnStatement *node = static_cast<ASTReturnStatement *>(nodePtr);
    std::optional<ASTNodePtr> expr = node->getExpr();

    std::ostringstream sourceOss;
    std::ostringstream headerOss;

    if (expr.has_value())
    {
        CodeGenCValuePtr exprValue = codeGenCExpression(scope, expr.value());

        headerOss << exprValue->getHeader();

        sourceOss << "return (";
        sourceOss << exprValue->getSource();
        sourceOss << ");";

        delete exprValue;
    }
    else
    {
        sourceOss << "return;";
    }

    return new CodeGenCValue(sourceOss.str(), headerOss.str(), CodeGenCValue::ValueType::Instruction);
}