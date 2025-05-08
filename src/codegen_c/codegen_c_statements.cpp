#include <sstream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr codeGenC_ReturnStatement(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_FunctionDefinition(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_FunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater = false);

std::pair<std::string, std::string> codeGenCStatement(ASTNodePtr statement)
{
    CodeGenCValuePtr value = nullptr;

    switch (statement->getType())
    {
    case ASTNode::NodeType::VariableDeclaration:
        value = codeGenC_VariableDeclaration(statement);
        break;
    case ASTNode::NodeType::FunctionDefinition:
        value = codeGenC_FunctionDefinition(statement);
        break;
    case ASTNode::NodeType::ReturnStatement:
        value = codeGenC_ReturnStatement(statement);
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
        //     auto [source, header] = codeGenCStatementList(static_cast<ASTStatementList *>(statement)->getStatements());
        //     value = new CodeGenCValue(source, header, CodeGenCValue::ValueType::Instruction);
        break;
    case ASTNode::NodeType::ImportStatement:
        std::cerr << "Cannot build import inside an another statement. It must be defined at the top level of your program." << std::endl;
        exit(1);
    case ASTNode::NodeType::Program:
        std::cerr << "Unable to generate C code for top level node ASTProgram." << std::endl;
        exit(1);
        break;
    default:
        std::cerr << "Unable to generate C code for unknown ASTNode." << std::endl;
        exit(1);
        break;
    }

    return std::make_pair(value->getSource(), value->getHeader());
}

CodeGenCValuePtr codeGenC_VariableDeclaration(ASTNodePtr nodePtr)
{
    ASTVariableDeclaration *node = static_cast<ASTVariableDeclaration *>(nodePtr);
    CodeGenCValuePtr typeValue = codeGenC_TypeSpecifier(node->getTypeValue());

    std::ostringstream oss;

    oss << typeValue->getSource() << " " << node->getName();
    if (node->getInitializer())
    {
        CodeGenCValuePtr exprValue = codeGenCExpression(node->getInitializer());
        oss << " = " << exprValue->getSource();
        delete exprValue;
    }

    oss << ";" << "\n";

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

    CodeGenCValuePtr exprValue = codeGenCExpression(node->getExpr());
    std::ostringstream funcDeclOss;

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
        funcDeclOss << ";";
    }

    return new CodeGenCValue(std::string(), funcDeclOss.str(), CodeGenCValue::ValueType::Instruction);
}

CodeGenCValuePtr codeGenC_FunctionDefinition(ASTNodePtr nodePtr)
{
    ASTFunctionDefinition *node = static_cast<ASTFunctionDefinition *>(nodePtr);

    std::ostringstream funcDeclOss;
    ASTFunctionDeclaration *funcDecl = ASTFunctionDeclaration::fromFunctionDefinition(*node);
    CodeGenCValuePtr funcDeclValue = codeGenC_FunctionDeclaration(funcDecl, true);
    funcDeclOss << funcDeclValue->getHeader() << ";\n";

    std::ostringstream funcDefOss;
    funcDefOss << funcDeclValue->getHeader() << " {\n";

    ASTStatementList *funcBody = static_cast<ASTStatementList *>(node->getBody());
    auto [bodySource, bodyHeaders] = codeGenCStatementList(funcBody->getStatements());
    funcDefOss << bodySource;

    funcDefOss << "}\n";
    funcDeclOss << bodyHeaders;

    return new CodeGenCValue(funcDefOss.str(), funcDeclOss.str(), CodeGenCValue::ValueType::Instruction);
}

CodeGenCValuePtr codeGenC_ReturnStatement(ASTNodePtr nodePtr)
{
    ASTReturnStatement *node = static_cast<ASTReturnStatement *>(nodePtr);
    std::optional<ASTNodePtr> expr = node->getExpr();

    std::ostringstream sourceOss;
    std::ostringstream headerOss;

    if (expr.has_value())
    {   
        CodeGenCValuePtr exprValue = codeGenCExpression(expr.value());

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