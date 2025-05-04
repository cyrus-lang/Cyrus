#ifndef AST_H
#define AST_H

#include <iostream>
#include <vector>
#include <memory>
#include "node.hpp"
#include "types.hpp"

class ASTProgram : public ASTNode
{
private:
    ASTNodeList statements_;

public:
    ASTProgram(ASTNodeList statements) : statements_(statements) {}
    NodeType getType() const override { return NodeType::Program; }
    const ASTNodeList &getStatements() const { return statements_; }
    void addStatement(ASTNodePtr statement) { statements_.push_back(statement); }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Program:" << std::endl;
        for (const auto &stmt : statements_)
        {
            stmt->print(indent + 1);
        }
    }
};

class ASTIntegerLiteral : public ASTNode
{
private:
    int value_;

public:
    ASTIntegerLiteral(int value) : value_(value) {}
    NodeType getType() const override { return NodeType::IntegerLiteral; }
    int getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "IntegerLiteral: " << value_;
    }
};

class ASTFloatLiteral : public ASTNode
{
private:
    float value_;

public:
    ASTFloatLiteral(float value) : value_(value) {}
    NodeType getType() const override { return NodeType::FloatLiteral; }
    float getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FloatLiteral: " << value_;
    }
};

class ASTStringLiteral : public ASTNode
{
private:
    std::string value_;

public:
    ASTStringLiteral(std::string value) : value_(value) {}
    NodeType getType() const override { return NodeType::StringLiteral; }
    std::string getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StringLiteral: " << value_;
    }
};

class ASTIdentifier : public ASTNode
{
public:
    ASTIdentifier(std::string name) : name_(name) {}
    NodeType getType() const override { return NodeType::Identifier; }
    const std::string &getName() const { return name_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Identifier: " << name_ << std::endl;
    }

private:
    std::string name_;
};

class ASTBinaryExpression : public ASTNode
{
public:
    enum class Operator
    {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        Equal,
        NotEqual,
        LessThan,
        LessEqual,
        GreaterThan,
        GreaterEqual,
        LeftShift,
        RightShift,
        BitwiseAnd,
        BitwiseXor,
        BitwiseOr,
        LogicalAnd,
        LogicalOr
    };

    ASTBinaryExpression(ASTNodePtr left, Operator op, ASTNodePtr right)
        : left_(left), op_(op), right_(right) {}

    NodeType getType() const override { return NodeType::BinaryExpression; }

    ASTNode *getLeft() const { return left_; }
    Operator getOperator() const { return op_; }
    ASTNode *getRight() const { return right_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "BinaryExpression: " << formatOperator(op_) << std::endl;
        left_->print(indent + 1);
        right_->print(indent + 1);
    }

private:
    ASTNodePtr left_;
    Operator op_;
    ASTNodePtr right_;

    std::string formatOperator(Operator op) const
    {
        switch (op)
        {
        case Operator::Add:
            return "+";
        case Operator::Subtract:
            return "-";
        case Operator::Multiply:
            return "*";
        case Operator::Divide:
            return "/";
        case Operator::Remainder:
            return "%";
        case Operator::Equal:
            return "==";
        case Operator::NotEqual:
            return "!=";
        case Operator::LessThan:
            return "<";
        case Operator::LessEqual:
            return "<=";
        case Operator::GreaterThan:
            return ">";
        case Operator::GreaterEqual:
            return ">=";
        case Operator::LeftShift:
            return "<<";
        case Operator::RightShift:
            return ">>";
        case Operator::BitwiseAnd:
            return "&";
        case Operator::BitwiseXor:
            return "^";
        case Operator::BitwiseOr:
            return "|";
        case Operator::LogicalAnd:
            return "&&";
        case Operator::LogicalOr:
            return "||";
        default:
            return "Unknown Operator";
        }
    }
};

class ASTImportStatement : public ASTNode
{
private:
    std::vector<std::string> modulePath_;

public:
    ASTImportStatement(std::vector<std::string> modulePath) : modulePath_(modulePath) {}
    NodeType getType() const override { return NodeType::ImportStatement; }
    const std::vector<std::string> &getModulePath() const { return modulePath_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ImportStatement: ";
        for (size_t i = 0; i < modulePath_.size(); ++i)
        {
            std::cout << modulePath_[i];
            if (i < modulePath_.size() - 1)
            {
                std::cout << "::";
            }
        }
        std::cout << std::endl;
    }
};

class ASTFunctionParameter : public ASTNode
{
private:
    std::string param_name_;
    ASTTypeSpecifier param_type_;
    ASTNodePtr default_value_;

public:
    ASTFunctionParameter(std::string param_name, ASTTypeSpecifier param_type, ASTNodePtr default_value = nullptr)
        : param_name_(param_name), param_type_(param_type), default_value_(default_value) {}

    NodeType getType() const override { return NodeType::FunctionParameter; }
    const std::string &getParamName() const { return param_name_; }
    ASTTypeSpecifier getParamType() const { return param_type_; }
    ASTNodePtr getDefaultValue() const { return default_value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionParameter: " << param_name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Type: ";
        param_type_.print(indent);
        std::cout << std::endl;

        if (default_value_)
        {
            printIndent(indent + 1);
            std::cout << "Default Value:" << std::endl;
            default_value_->print(indent + 2);
        }
    }
};

class ASTFunctionDefinition : public ASTNode
{
private:
    std::string name_;
    std::vector<ASTFunctionParameter> parameters_;
    ASTTypeSpecifier returnType_;
    ASTNodePtr body_;

public:
    ASTFunctionDefinition(std::string name, std::vector<ASTFunctionParameter> parameters, ASTTypeSpecifier returnType, ASTNodePtr body)
        : name_(name), parameters_(parameters), returnType_(returnType), body_(body) {}

    NodeType getType() const override { return NodeType::FunctionDefinition; }
    const std::string &getName() const { return name_; }
    const std::vector<ASTFunctionParameter> &getParameters() const { return parameters_; }
    ASTTypeSpecifier getReturnType() const { return returnType_; }
    ASTNode *getBody() const { return body_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionDefinition: " << name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Parameters:" << std::endl;
        for (const auto &param : parameters_)
        {
            param.print(indent + 2);
        }

        printIndent(indent + 1);
        std::cout << "Return Type: ";
        returnType_.print(indent);
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "Body:" << std::endl;
        body_->print(indent + 2);
    }
};

class ASTVariableDeclaration : public ASTNode
{
private:
    std::string name_;
    ASTTypeSpecifier* type_;
    ASTNodePtr initializer_;

public:
    ASTVariableDeclaration(std::string name, ASTTypeSpecifier* type, ASTNodePtr initializer = nullptr)
        : name_(name), type_(type), initializer_(initializer) {}

    NodeType getType() const override { return NodeType::VariableDeclaration; }
    const std::string &getName() const { return name_; }
    ASTTypeSpecifier* getTypeValue() const { return type_; }
    ASTNodePtr getInitializer() const { return initializer_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "VariableDeclaration: " << name_ << std::endl;

        if (type_) {
            printIndent(indent + 1);
            type_->print(indent);
            std::cout << std::endl;
        }

        if (initializer_)
        {
            printIndent(indent + 1);
            std::cout << "Initializer:" << std::endl;
            initializer_->print(indent + 2);
        }
    }
};

#endif // AST_H