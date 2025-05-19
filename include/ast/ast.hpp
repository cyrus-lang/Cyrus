#ifndef AST_HPP
#define AST_HPP

#include <nlohmann/json.hpp>
#include <iostream>
#include <vector>
#include <memory>
#include "node.hpp"
#include "types.hpp"

class ASTStatementList : public ASTNode
{
private:
    ASTNodeList statements_;
    std::size_t lineNumber_;

public:
    ASTStatementList(std::size_t lineNumber) : statements_(), lineNumber_(lineNumber) {}
    ASTStatementList(ASTNodePtr statement, std::size_t lineNumber) : statements_({statement}), lineNumber_(lineNumber) {}
    ~ASTStatementList()
    {
        for (auto &statement : statements_)
        {
            delete statement;
        }
    }
    NodeType getType() const override { return NodeType::StatementList; }
    const ASTNodeList &getStatements() const { return statements_; }
    void addStatement(ASTNodePtr statement) { statements_.push_back(statement); }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StatementList:" << std::endl;
        for (const auto &stmt : statements_)
        {
            stmt->print(indent + 1);
        }
    }
};

class ASTProgram : public ASTNode
{
private:
    ASTStatementList *statementList_;

public:
    ASTProgram()
    {
        statementList_ = new ASTStatementList(0);
    }
    ~ASTProgram()
    {
        delete statementList_;
    }

    NodeType getType() const override { return NodeType::Program; }

    ASTStatementList *getStatementList()
    {
        return statementList_;
    }

    // TODO
    nlohmann::json jsonify() const override {}

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Program:" << std::endl;
        statementList_->print(indent + 1);
    }
};

enum class ASTAccessSpecifier
{
    Default,
    Public,
    Private,
    Abstract,
    Virtual,
    Override,
    Protected
};

enum class ASTStorageClassSpecifier
{
    Extern,
    Inline,
};

const std::string formatAccessSpecifier(ASTAccessSpecifier accessSpecifier);
void printAccessSpecifier(ASTAccessSpecifier accessSpecifier);

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

class ASTBoolLiteral : public ASTNode
{
private:
    bool value_;

public:
    ASTBoolLiteral(bool value) : value_(value) {}
    NodeType getType() const override { return NodeType::BoolLiteral; }
    bool getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "BoolLiteral: ";
        if (value_)
        {
            std::cout << "true";
        }
        else
        {
            std::cout << "false";
        }
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
private:
    std::string name_;
    std::size_t lineNumber_;

public:
    ASTIdentifier(std::string name, std::size_t lineNumber) : name_(name), lineNumber_(lineNumber) {}
    NodeType getType() const override { return NodeType::Identifier; }
    const std::string &getName() const { return name_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Identifier: " << name_ << std::endl;
    }
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

    ASTBinaryExpression(ASTNodePtr left, Operator op, ASTNodePtr right, std::size_t lineNumber)
        : left_(left), op_(op), right_(right), lineNumber_(lineNumber) {}
    ~ASTBinaryExpression()
    {
        delete left_;
        delete right_;
    }

    NodeType getType() const override { return NodeType::BinaryExpression; }

    ASTNodePtr getLeft() const { return left_; }
    Operator getOperator() const { return op_; }
    ASTNodePtr getRight() const { return right_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "BinaryExpression: " << formatOperator(op_) << std::endl;
        left_->print(indent + 1);
        std::cout << std::endl;
        right_->print(indent + 1);
    }

private:
    ASTNodePtr left_;
    Operator op_;
    ASTNodePtr right_;
    std::size_t lineNumber_;

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

class ASTUnaryExpression : public ASTNode
{
public:
    enum class Operator
    {
        Plus,
        Negate,
        LogicalNot,
        BitwiseNot,
        AddressOf,
        Dereference,
        PreIncrement,
        PreDecrement,
        PostIncrement,
        PostDecrement
    };

    ASTUnaryExpression(Operator op, ASTNodePtr operand, std::size_t lineNumber)
        : op_(op), operand_(operand), lineNumber_(lineNumber) {}
    ~ASTUnaryExpression()
    {
        delete operand_;
    }

    NodeType getType() const override { return NodeType::UnaryExpression; }

    Operator getOperator() const { return op_; }
    ASTNodePtr getOperand() const { return operand_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "UnaryExpression: " << formatOperator(op_) << std::endl;
        operand_->print(indent + 1);
    }

private:
    Operator op_;
    ASTNodePtr operand_;
    std::size_t lineNumber_;

    std::string formatOperator(Operator op) const
    {
        switch (op)
        {
        case Operator::Negate:
            return "-";
        case Operator::LogicalNot:
            return "!";
        case Operator::BitwiseNot:
            return "~";
        case Operator::Plus:
            return "+";
        case Operator::AddressOf:
            return "&";
        case Operator::Dereference:
            return "*";
        case Operator::PreIncrement:
            return "++";
        case Operator::PreDecrement:
            return "--";
        case Operator::PostIncrement:
            return "++";
        case Operator::PostDecrement:
            return "--";
        default:
            return "Unknown Operator";
        }
    }
};

class ASTCastExpression : public ASTNode
{
private:
    ASTNodePtr expression_;
    ASTTypeSpecifier targetType_;
    std::size_t lineNumber_;

public:
    ASTCastExpression(ASTTypeSpecifier targetType, ASTNodePtr expression, std::size_t lineNumber)
        : expression_(expression), targetType_(targetType), lineNumber_(lineNumber) {}
    ~ASTCastExpression()
    {
        delete expression_;
    }

    NodeType getType() const override { return NodeType::CastExpression; }
    ASTNodePtr getExpression() const { return expression_; }
    ASTTypeSpecifier getTargetType() const { return targetType_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "CastExpression:" << std::endl;

        printIndent(indent + 1);
        std::cout << "Target Type: ";
        targetType_.print(indent + 1);
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "Expression:" << std::endl;
        expression_->print(indent + 2);
    }
};

class ASTImportStatement : public ASTNode
{
private:
    std::vector<std::string> modulePath_;
    std::size_t lineNumber_;

public:
    ASTImportStatement(std::vector<std::string> modulePath, std::size_t lineNumber) : modulePath_(modulePath), lineNumber_(lineNumber) {}
    NodeType getType() const override { return NodeType::ImportStatement; }
    const std::vector<std::string> &getModulePath() const { return modulePath_; }
    std::size_t getLineNumber() const { return lineNumber_; }

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

class ASTImportedSymbolAccess : public ASTNode
{
private:
    std::vector<std::string> symbolPath_;
    std::size_t lineNumber_;

public:
    ASTImportedSymbolAccess(std::vector<std::string> symbolPath, std::size_t lineNumber) : symbolPath_(symbolPath), lineNumber_(lineNumber) {}
    NodeType getType() const override { return NodeType::ImportedSymbolAccess; }
    const std::vector<std::string> &getSymbolPath() const { return symbolPath_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ImportedSymbolAccess: ";
        for (size_t i = 0; i < symbolPath_.size(); ++i)
        {
            std::cout << symbolPath_[i];
            if (i < symbolPath_.size() - 1)
            {
                std::cout << "::";
            }
        }
        std::cout << std::endl;
    }
};

class ASTTypeDefStatement : public ASTNode
{
private:
    std::string name_;
    ASTTypeSpecifier type_;
    ASTAccessSpecifier accessSpecifier_;
    std::size_t lineNumber_;

public:
    ASTTypeDefStatement(std::string name, ASTTypeSpecifier type, std::size_t lineNumber, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)
        : name_(name), type_(type), accessSpecifier_(accessSpecifier), lineNumber_(lineNumber) {}

    NodeType getType() const override { return NodeType::TypeDefStatement; }
    const std::string &getName() const { return name_; }
    const ASTTypeSpecifier &getTypeSpecifier() const { return type_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "TypeDefStatement: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_;
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "Type: ";
        type_.print(indent);

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);
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
    ~ASTFunctionParameter()
    {
        delete default_value_;
    }

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

class ASTFunctionParameters : public ASTNode
{
private:
    std::vector<ASTFunctionParameter> parameters_;
    std::optional<ASTTypeSpecifier *> typed_variadic_;
    bool is_variadic_;

public:
    ASTFunctionParameters(std::vector<ASTFunctionParameter> parameters, std::optional<ASTTypeSpecifier *> typed_variadic = std::nullopt, bool is_variadic = false)
        : parameters_(parameters), typed_variadic_(typed_variadic), is_variadic_(is_variadic) {}
    ~ASTFunctionParameters()
    {
        if (typed_variadic_.has_value())
        {
            delete typed_variadic_.value();
        }
    }

    NodeType getType() const override { return NodeType::FunctionParameter; }
    const std::vector<ASTFunctionParameter> &getList() const { return parameters_; }
    std::optional<ASTTypeSpecifier *> getTypedVariadic() const { return typed_variadic_; }
    bool getIsVariadic() const { return is_variadic_; }

    void addParameter(ASTFunctionParameter param)
    {
        parameters_.push_back(param);
    }

    void setVariadic(std::optional<ASTTypeSpecifier *> typed_variadic)
    {
        typed_variadic_ = typed_variadic;
        is_variadic_ = true;
    }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionParameters: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Parameters:" << std::endl;
        for (const auto &param : parameters_)
        {
            param.print(indent + 2);
        }

        if (typed_variadic_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Typed Variadic: ";
            typed_variadic_.value()->print(indent);
            std::cout << std::endl;
        }

        printIndent(indent + 1);
        std::cout << "Is Variadic: " << is_variadic_ << std::endl;
    }
};

class ASTFunctionDefinition : public ASTNode
{
private:
    ASTNodePtr expr_;
    ASTFunctionParameters parameters_;
    ASTNodePtr body_;
    ASTAccessSpecifier accessSpecifier_;
    std::optional<ASTTypeSpecifier *> returnType_;
    std::optional<ASTStorageClassSpecifier> storageClassSpecifier_;
    std::size_t lineNumber_;

public:
    ASTFunctionDefinition(ASTNodePtr expr,
                          ASTFunctionParameters parameters,
                          std::optional<ASTTypeSpecifier *> returnType,
                          ASTNodePtr body,
                          std::size_t lineNumber,
                          ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default,
                          std::optional<ASTStorageClassSpecifier> storageClassSpecifier = std::nullopt)
        : expr_(expr), parameters_(parameters), body_(body),
          accessSpecifier_(accessSpecifier), returnType_(returnType), storageClassSpecifier_(storageClassSpecifier), lineNumber_(lineNumber) {}
    ~ASTFunctionDefinition()
    {
        delete expr_;
        delete body_;
        if (returnType_.has_value())
        {
            delete returnType_.value();
        }
    }

    NodeType getType() const override { return NodeType::FunctionDefinition; }
    ASTNodePtr getExpr() const { return expr_; }
    ASTFunctionParameters getParameters() const { return parameters_; }
    std::optional<ASTTypeSpecifier *> getReturnType() const { return returnType_; }
    ASTNodePtr getBody() const { return body_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::optional<ASTStorageClassSpecifier> getStorageClassSpecifier() const { return storageClassSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionDefinition: " << std::endl;
        expr_->print(indent + 1);

        printIndent(indent + 1);
        std::cout << "Parameters:" << std::endl;
        for (const auto &param : parameters_.getList())
        {
            param.print(indent + 2);
        }

        if (returnType_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Return Type: ";
            returnType_.value()->print(indent);
        }

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);

        if (storageClassSpecifier_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Storage Class Specifier: ";
            switch (storageClassSpecifier_.value())
            {
            case ASTStorageClassSpecifier::Extern:
                std::cout << "Extern";
                break;
            case ASTStorageClassSpecifier::Inline:
                std::cout << "Inline";
                break;
            default:
                std::cout << "Unknown";
                break;
            }
            std::cout << std::endl;
        }

        printIndent(indent + 1);
        std::cout << "Body:" << std::endl;
        body_->print(indent + 2);
    }
};

class ASTFunctionDeclaration : public ASTNode
{
private:
    ASTNodePtr expr_;
    ASTFunctionParameters parameters_;
    ASTAccessSpecifier accessSpecifier_;
    std::optional<ASTTypeSpecifier *> returnType_;
    std::optional<ASTStorageClassSpecifier> storageClassSpecifier_;
    std::size_t lineNumber_;

public:
    ASTFunctionDeclaration(ASTNodePtr expr,
                           ASTFunctionParameters parameters,
                           std::optional<ASTTypeSpecifier *> returnType,
                           std::size_t lineNumber,
                           ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default,
                           std::optional<ASTStorageClassSpecifier> storageClassSpecifier = std::nullopt)
        : expr_(expr), parameters_(parameters),
          accessSpecifier_(accessSpecifier), returnType_(returnType), storageClassSpecifier_(storageClassSpecifier), lineNumber_(lineNumber) {}
    ~ASTFunctionDeclaration()
    {
        delete expr_;
        if (returnType_.has_value())
        {
            delete returnType_.value();
        }
    }

    NodeType getType() const override { return NodeType::FunctionDeclaration; }
    ASTNodePtr getExpr() const { return expr_; }
    ASTFunctionParameters getParameters() const { return parameters_; }
    std::optional<ASTTypeSpecifier *> getReturnType() const { return returnType_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::optional<ASTStorageClassSpecifier> getStorageClassSpecifier() const { return storageClassSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionDeclaration: " << std::endl;
        expr_->print(indent + 1);

        printIndent(indent + 1);
        std::cout << "Parameters:" << std::endl;
        for (const auto &param : parameters_.getList())
        {
            param.print(indent + 2);
        }

        if (returnType_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Return Type: ";
            returnType_.value()->print(indent);
        }

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);

        if (storageClassSpecifier_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Storage Class Specifier: ";
            switch (storageClassSpecifier_.value())
            {
            case ASTStorageClassSpecifier::Extern:
                std::cout << "Extern";
                break;
            case ASTStorageClassSpecifier::Inline:
                std::cout << "Inline";
                break;
            default:
                std::cout << "Unknown";
                break;
            }
            std::cout << std::endl;
        }
    }

    static ASTFunctionDeclaration *fromFunctionDefinition(const ASTFunctionDefinition &functionDefinition, std::size_t lineNumber)
    {
        return new ASTFunctionDeclaration(functionDefinition.getExpr(), functionDefinition.getParameters(), functionDefinition.getReturnType(), lineNumber, functionDefinition.getAccessSpecifier(), functionDefinition.getStorageClassSpecifier());
    }
};

class ASTVariableDeclaration : public ASTNode
{
private:
    std::string name_;
    std::optional<ASTTypeSpecifier *> type_;
    std::optional<ASTNodePtr> initializer_;
    std::size_t lineNumber_;

public:
    ASTVariableDeclaration(std::string name, std::optional<ASTTypeSpecifier *> type, std::size_t lineNumber, std::optional<ASTNodePtr> initializer = std::nullopt)
        : name_(name), type_(type), initializer_(initializer), lineNumber_(lineNumber) {}
    ~ASTVariableDeclaration()
    {
        if (initializer_.has_value())
        {
            delete initializer_.value();
        }

        if (type_.has_value())
        {
            delete type_.value();
        }
    }

    NodeType getType() const override { return NodeType::VariableDeclaration; }
    const std::string &getName() const { return name_; }
    std::optional<ASTTypeSpecifier *> getTypeValue() const { return type_; }
    std::optional<ASTNodePtr> getInitializer() const { return initializer_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "VariableDeclaration: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_ << std::endl;

        if (type_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Type: ";
            type_.value()->print(indent);
        }

        if (initializer_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Initializer:" << std::endl;
            initializer_.value()->print(indent + 2);
            std::cout << std::endl;
        }
    }
};

class ASTGlobalVariableDeclaration : public ASTNode
{
private:
    std::string name_;
    std::optional<ASTTypeSpecifier *> type_;
    std::optional<ASTNodePtr> initializer_;
    ASTAccessSpecifier accessSpecifier_;
    std::optional<ASTStorageClassSpecifier> storageClassSpecifier_;
    std::size_t lineNumber_;

public:
    ASTGlobalVariableDeclaration(std::string name, std::optional<ASTTypeSpecifier *> type, std::optional<ASTNodePtr> initializer, std::size_t lineNumber, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default, std::optional<ASTStorageClassSpecifier> storageClassSpecifier = std::nullopt)
        : name_(name), type_(type), initializer_(initializer), accessSpecifier_(accessSpecifier), storageClassSpecifier_(storageClassSpecifier), lineNumber_(lineNumber) {}
    ~ASTGlobalVariableDeclaration()
    {
        if (initializer_.has_value())
        {
            delete initializer_.value();
        }

        if (type_.has_value())
        {
            delete type_.value();
        }
    }

    NodeType getType() const override { return NodeType::VariableDeclaration; }
    const std::string &getName() const { return name_; }
    std::optional<ASTTypeSpecifier *> getTypeValue() const { return type_; }
    std::optional<ASTNodePtr> getInitializer() const { return initializer_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::optional<ASTStorageClassSpecifier> getStorageClassSpecifier() const { return storageClassSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "GlobalVariableDeclaration: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_ << std::endl;

        if (type_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Type: ";
            type_.value()->print(indent);
        }

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);

        if (storageClassSpecifier_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Storage Class Specifier: ";
            switch (storageClassSpecifier_.value())
            {
            case ASTStorageClassSpecifier::Extern:
                std::cout << "Extern";
                break;
            case ASTStorageClassSpecifier::Inline:
                std::cout << "Inline";
                break;
            default:
                std::cout << "Unknown";
                break;
            }
            std::cout << std::endl;
        }

        if (initializer_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Initializer:" << std::endl;
            initializer_.value()->print(indent + 2);
            std::cout << std::endl;
        }
    }
};

class ASTStructField : public ASTNode
{
private:
    std::string name_;
    ASTTypeSpecifier type_;
    ASTAccessSpecifier accessSpecifier_;
    std::size_t lineNumber_;

public:
    ASTStructField(std::string name, ASTTypeSpecifier type, std::size_t lineNumber, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)
        : name_(name), type_(type), accessSpecifier_(accessSpecifier), lineNumber_(lineNumber) {}

    NodeType getType() const override { return NodeType::StructField; }
    const std::string &getName() const { return name_; }
    const ASTTypeSpecifier &getTypeSpecifier() const { return type_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StructField: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Type: ";
        type_.print(indent);

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);
    }
};

class ASTStructDefinition : public ASTNode
{
private:
    std::optional<std::string> name_;
    std::vector<ASTStructField> members_;
    std::vector<ASTFunctionDefinition> methods_;
    ASTAccessSpecifier accessSpecifier_;
    std::size_t lineNumber_;

public:
    ASTStructDefinition(std::optional<std::string> name, std::vector<ASTStructField> members, std::vector<ASTFunctionDefinition> methods, std::size_t lineNumber, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)
        : name_(name), members_(members), methods_(methods), accessSpecifier_(accessSpecifier), lineNumber_(lineNumber) {}

    NodeType getType() const override { return NodeType::StructDefinition; }
    const std::optional<std::string> &getName() const { return name_; }
    const std::vector<ASTStructField> &getMembers() const { return members_; }
    const std::vector<ASTFunctionDefinition> &getMethods() const { return methods_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StructDefinition: " << std::endl;

        if (name_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Name: " << name_.value() << std::endl;
        }

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);

        printIndent(indent + 1);
        std::cout << "Members:" << std::endl;
        for (const auto &member : members_)
        {
            member.print(indent + 2);
        }

        printIndent(indent + 1);
        std::cout << "Methods:" << std::endl;
        for (const auto &method : methods_)
        {
            method.print(indent + 2);
        }
    }
};

class ASTStructInitialization : public ASTNode
{
private:
    std::string structName_;
    std::vector<std::pair<std::string, ASTNodePtr>> fieldInitializers_;
    std::size_t lineNumber_;

public:
    ASTStructInitialization(std::string structName, std::vector<std::pair<std::string, ASTNodePtr>> fieldInitializers, std::size_t lineNumber)
        : structName_(structName), fieldInitializers_(fieldInitializers), lineNumber_(lineNumber) {}
    ~ASTStructInitialization()
    {
        for (auto &&item : fieldInitializers_)
        {
            delete item.second;
        }
    }

    NodeType getType() const override { return NodeType::StructInitialization; }
    const std::string &getStructName() const { return structName_; }
    const std::vector<std::pair<std::string, ASTNodePtr>> &getFieldInitializers() const { return fieldInitializers_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StructInitialization: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Struct Name: " << structName_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Field Initializers:" << std::endl;
        for (const auto &pair : fieldInitializers_)
        {
            printIndent(indent + 2);
            std::cout << "Field Name: " << pair.first << std::endl;
            pair.second->print(indent + 3);
        }
    }
};

class ASTConditionalExpression : public ASTNode
{
private:
    ASTNodePtr condition_;
    ASTNodePtr trueExpression_;
    ASTNodePtr falseExpression_;
    std::size_t lineNumber_;

public:
    ASTConditionalExpression(ASTNodePtr condition, ASTNodePtr trueExpression, ASTNodePtr falseExpression, std::size_t lineNumber)
        : condition_(condition), trueExpression_(trueExpression), falseExpression_(falseExpression), lineNumber_(lineNumber) {}
    ~ASTConditionalExpression()
    {
        delete condition_;
        delete trueExpression_;
        delete falseExpression_;
    }

    NodeType getType() const override { return NodeType::ConditionalExpression; }
    ASTNodePtr getCondition() const { return condition_; }
    ASTNodePtr getTrueExpression() const { return trueExpression_; }
    ASTNodePtr getFalseExpression() const { return falseExpression_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ConditionalExpression:" << std::endl;

        printIndent(indent + 1);
        std::cout << "Condition:" << std::endl;
        condition_->print(indent + 2);

        printIndent(indent + 1);
        std::cout << "True Expression:" << std::endl;
        trueExpression_->print(indent + 2);
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "False Expression:" << std::endl;
        falseExpression_->print(indent + 2);
    }
};

class ASTAssignment : public ASTNode
{
public:
    enum class Operator
    {
        Assign,
        AddAssign,
        SubtractAssign,
        MultiplyAssign,
        DivideAssign,
        RemainderAssign,
        LeftShiftAssign,
        RightShiftAssign,
        BitwiseAndAssign,
        BitwiseXorAssign,
        BitwiseOrAssign
    };

    ASTAssignment(ASTNodePtr left, Operator op, ASTNodePtr right, std::size_t lineNumber)
        : left_(left), op_(op), right_(right), lineNumber_(lineNumber) {}
    ~ASTAssignment()
    {
        delete left_;
        delete right_;
    }

    NodeType getType() const override { return NodeType::AssignmentExpression; }

    ASTNodePtr getLeft() const { return left_; }
    ASTNodePtr getRight() const { return right_; }
    Operator getOperator() const { return op_; }
    std::string getOperatorString() const { return formatOperator(op_); }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "AssignmentExpression: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Operator: " << formatOperator(op_) << std::endl;

        left_->print(indent + 1);
        right_->print(indent + 1);
    }

private:
    ASTNodePtr left_;
    Operator op_;
    ASTNodePtr right_;
    std::size_t lineNumber_;

    std::string formatOperator(Operator op) const
    {
        switch (op)
        {
        case Operator::Assign:
            return "=";
        case Operator::AddAssign:
            return "+=";
        case Operator::SubtractAssign:
            return "-=";
        case Operator::MultiplyAssign:
            return "*=";
        case Operator::DivideAssign:
            return "/=";
        case Operator::RemainderAssign:
            return "%=";
        case Operator::LeftShiftAssign:
            return "<<=";
        case Operator::RightShiftAssign:
            return ">>=";
        case Operator::BitwiseAndAssign:
            return "&=";
        case Operator::BitwiseXorAssign:
            return "^=";
        case Operator::BitwiseOrAssign:
            return "|=";
        default:
            return "Unknown Operator";
        }
    }
};

class ASTFunctionCall : public ASTNode
{
private:
    ASTNodePtr expr_;
    std::vector<ASTNodePtr> arguments_;
    std::size_t lineNumber_;

public:
    ASTFunctionCall(ASTNodePtr expr, std::vector<ASTNodePtr> arguments, std::size_t lineNumber)
        : expr_(expr), arguments_(arguments), lineNumber_(lineNumber) {}
    ~ASTFunctionCall()
    {
        for (auto &&argument : arguments_)
        {
            delete argument;
        }
        delete expr_;
    }

    NodeType getType() const override { return NodeType::FunctionCall; }
    ASTNodePtr getExpr() const { return expr_; }
    const std::vector<ASTNodePtr> &getArguments() const { return arguments_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionCall: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Function Expr:" << std::endl;
        expr_->print(indent + 2);

        printIndent(indent + 1);
        std::cout << "Arguments:" << std::endl;
        for (const auto &arg : arguments_)
        {
            arg->print(indent + 2);
        }
    }
};

class ASTFieldAccess : public ASTNode
{
private:
    ASTNodePtr operand_;
    std::string field_name_;
    std::size_t lineNumber_;

public:
    ASTFieldAccess(ASTNodePtr operand, std::string field_name, std::size_t lineNumber)
        : operand_(operand), field_name_(field_name), lineNumber_(lineNumber) {}
    ~ASTFieldAccess()
    {
        delete operand_;
    }

    NodeType getType() const override { return NodeType::FieldAccess; }
    ASTNodePtr getOperand() const { return operand_; }
    const std::string &getFieldName() const { return field_name_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FieldAccess: " << field_name_ << std::endl;
        operand_->print(indent + 1);
    }
};

class ASTPointerFieldAccess : public ASTNode
{
private:
    ASTFieldAccess field_access_;
    std::size_t lineNumber_;

public:
    ASTPointerFieldAccess(ASTFieldAccess field_access, std::size_t lineNumber) : field_access_(field_access), lineNumber_(lineNumber) {}
    NodeType getType() const override { return NodeType::PointerFieldAccess; }
    ASTFieldAccess getFieldAccess() const { return field_access_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "PointerFieldAccess: " << std::endl;
        field_access_.print(indent + 1);
    }
};

class ASTEnumVariantItem
{
private:
    std::optional<std::string> name_;
    ASTTypeSpecifier type_;
    std::size_t lineNumber_;

public:
    ASTEnumVariantItem(std::optional<std::string> name, ASTTypeSpecifier type, std::size_t lineNumber) : name_(name), type_(type), lineNumber_(lineNumber) {}

    const std::optional<std::string> &getName() const { return name_; }
    const ASTTypeSpecifier &getTypeSpecifier() const { return type_; }
    std::size_t getLineNumber() const { return lineNumber_; }
};

class ASTEnumVariant : public ASTNode
{
private:
    std::string name_;
    std::vector<ASTEnumVariantItem> items_;
    std::size_t lineNumber_;

public:
    ASTEnumVariant(std::string name, std::vector<ASTEnumVariantItem> items, std::size_t lineNumber)
        : name_(name), items_(items), lineNumber_(lineNumber) {}

    NodeType getType() const override { return NodeType::EnumVariant; }
    const std::string &getName() const { return name_; }
    const std::vector<ASTEnumVariantItem> &getItems() const { return items_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "EnumVariant: " << std::endl;
        printIndent(indent + 1);

        std::cout << "Name: " << name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Items:" << std::endl;
        for (const auto &item : items_)
        {
            printIndent(indent + 2);
            std::cout << "Item: " << std::endl;
            if (item.getName().has_value())
            {
                printIndent(indent + 3);
                std::cout << "Name: " << item.getName().value() << std::endl;
            }
            printIndent(indent + 3);
            std::cout << "Type: ";
            item.getTypeSpecifier().print(indent + 3);
        }
    }
};

class ASTEnumDefinition : public ASTNode
{
private:
    std::optional<std::string> name_;
    std::vector<ASTEnumVariant> variants_;
    std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> fields_;
    std::vector<ASTFunctionDefinition> methods_;
    ASTAccessSpecifier accessSpecifier_;
    std::size_t lineNumber_;

public:
    ASTEnumDefinition(std::optional<std::string> name,
                      std::vector<ASTEnumVariant> variants,
                      std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> fields,
                      std::vector<ASTFunctionDefinition> methods,
                      std::size_t lineNumber,
                      ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)

        : name_(name), variants_(variants), fields_(fields), methods_(methods), accessSpecifier_(accessSpecifier), lineNumber_(lineNumber)
    {
    }
    ~ASTEnumDefinition()
    {
        for (auto &&item : fields_)
        {
            if (item.second.has_value())
            {
                delete item.second.value();
            }
        }
    }

    NodeType getType() const override { return NodeType::EnumDefinition; }
    const std::optional<std::string> &getName() const { return name_; }
    const std::vector<ASTEnumVariant> &getVariants() const { return variants_; }
    const std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> &getFields() const { return fields_; }
    const std::vector<ASTFunctionDefinition> &getMethods() const { return methods_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "EnumDefinition: " << std::endl;

        if (name_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Name: " << name_.value() << std::endl;
        }

        printIndent(indent + 1);
        printAccessSpecifier(accessSpecifier_);

        printIndent(indent + 1);
        std::cout << "Variants:" << std::endl;
        for (const auto &variant : variants_)
        {
            variant.print(indent + 2);
        }

        printIndent(indent + 1);
        std::cout << "Fields:" << std::endl;
        for (const auto &field : fields_)
        {
            std::optional<ASTNodePtr> fieldValue = field.second;
            if (fieldValue.has_value())
            {
                printIndent(indent + 2);
                std::cout << field.first << ": " << std::endl;
                fieldValue.value()->print(indent + 3);
                std::cout << std::endl;
            }
        }

        printIndent(indent + 1);
        std::cout << "Methods:" << std::endl;
        for (const auto &method : methods_)
        {
            method.print(indent + 2);
        }
    }
};

class ASTReturnStatement : public ASTNode
{
private:
    std::optional<ASTNodePtr> expression_;
    std::size_t lineNumber_;

public:
    ASTReturnStatement(std::optional<ASTNodePtr> expression, std::size_t lineNumber) : expression_(expression), lineNumber_(lineNumber) {}
    ~ASTReturnStatement()
    {
        if (expression_.has_value())
        {
            delete expression_.value();
        }
    }

    NodeType getType() const override { return NodeType::ReturnStatement; }
    const std::optional<ASTNodePtr> &getExpr() const { return expression_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        if (expression_.has_value())
        {
            printIndent(indent);
            std::cout << "ReturnStatement: " << std::endl;
            expression_.value()->print(indent + 1);
        }
        else
        {
            printIndent(indent);
            std::cout << "ReturnStatement" << std::endl;
        }
    }
};

class ASTBreakStatement : public ASTNode
{
private:
    std::size_t lineNumber_;

public:
    ASTBreakStatement(std::size_t lineNumber) : lineNumber_(lineNumber) {}

    NodeType getType() const override { return NodeType::BreakStatement; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "BreakStatement" << std::endl;
    }
};

class ASTContinueStatement : public ASTNode
{
private:
    std::size_t lineNumber_;

public:
    ASTContinueStatement(std::size_t lineNumber) : lineNumber_(lineNumber) {}

    NodeType getType() const override { return NodeType::ContinueStatement; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ContinueStatement" << std::endl;
    }
};

class ASTForStatement : public ASTNode
{
private:
    std::optional<ASTNodePtr> initializer_;
    std::optional<ASTNodePtr> condition_;
    std::optional<ASTNodePtr> increment_;
    ASTNodePtr body_;
    std::size_t lineNumber_;

public:
    ASTForStatement(std::optional<ASTNodePtr> initializer, std::optional<ASTNodePtr> condition, std::optional<ASTNodePtr> increment, ASTNodePtr body, std::size_t lineNumber)
        : initializer_(initializer), condition_(condition), increment_(increment), body_(body), lineNumber_(lineNumber) {}
    ~ASTForStatement()
    {
        if (initializer_.has_value())
        {
            delete initializer_.value();
        }
        if (condition_.has_value())
        {
            delete condition_.value();
        }
        if (increment_.has_value())
        {
            delete increment_.value();
        }
        delete body_;
    }

    NodeType getType() const override { return NodeType::ForStatement; }
    std::optional<ASTNodePtr> getInitializer() const { return initializer_; }
    std::optional<ASTNodePtr> getCondition() const { return condition_; }
    std::optional<ASTNodePtr> getIncrement() const { return increment_; }
    ASTNodePtr getBody() const { return body_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ForStatement: " << std::endl;

        if (initializer_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Initializer:" << std::endl;
            initializer_.value()->print(indent + 2);
        }

        if (condition_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Condition:" << std::endl;
            condition_.value()->print(indent + 2);
        }

        if (increment_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Increment:" << std::endl;
            increment_.value()->print(indent + 2);
        }

        printIndent(indent + 1);
        std::cout << "Body:" << std::endl;
        body_->print(indent + 2);
    }
};

class ASTIfStatement : public ASTNode
{
private:
    ASTNodePtr condition_;
    ASTNodePtr thenBranch_;
    std::optional<ASTNodePtr> elseBranch_;
    std::size_t lineNumber_;

public:
    ASTIfStatement(ASTNodePtr condition, ASTNodePtr thenBranch, std::size_t lineNumber, std::optional<ASTNodePtr> elseBranch = std::nullopt)
        : condition_(condition), thenBranch_(thenBranch), elseBranch_(elseBranch), lineNumber_(lineNumber) {}
    ~ASTIfStatement()
    {
        delete condition_;
        delete thenBranch_;
        if (elseBranch_.has_value())
        {
            delete elseBranch_.value();
        }
    }

    NodeType getType() const override { return NodeType::IfStatement; }
    ASTNodePtr getCondition() const { return condition_; }
    ASTNodePtr getThenBranch() const { return thenBranch_; }
    std::optional<ASTNodePtr> getElseBranch() const { return elseBranch_; }
    std::size_t getLineNumber() const { return lineNumber_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "IfStatement:" << std::endl;

        printIndent(indent + 1);
        std::cout << "Condition:" << std::endl;
        condition_->print(indent + 2);
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "Then Branch:" << std::endl;
        thenBranch_->print(indent + 2);

        if (elseBranch_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Else Branch:" << std::endl;
            elseBranch_.value()->print(indent + 2);
        }
    }
};

#endif