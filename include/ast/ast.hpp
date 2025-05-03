#ifndef AST_H
#define AST_H

#include <iostream>
#include <vector>
#include <memory>

class ASTNode
{
public:
    enum class NodeType
    {
        Program,
        ImportModule,
        VariableDeclaration,
        IntegerLiteral,
        FloatLiteral,
        StringLiteral,
        Identifier,
        BinaryExpression,
    };

    virtual ~ASTNode() = default;
    virtual NodeType getType() const = 0;

    virtual void print(int) const {}

protected:
    void printIndent(int indent) const
    {
        for (int i = 0; i < indent; ++i)
        {
            std::cout << "  ";
        }
    }
};

using ASTNodePtr = ASTNode *;
using ASTNodeList = std::vector<ASTNodePtr>;

class Program : public ASTNode
{
private:
    ASTNodeList statements_;

public:
    Program(ASTNodeList statements) : statements_(statements) {}
    NodeType getType() const override { return NodeType::Program; }
    const ASTNodeList &getStatements() const { return statements_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Program:" << std::endl;
        std::cout << "Program:" << std::endl;
        for (const auto &stmt : statements_)
        {
            stmt->print(indent + 1);
        }
    }
};
class IntegerLiteral : public ASTNode
{
private:
    int value_;

public:
    IntegerLiteral(int value) : value_(value) {}
    NodeType getType() const override { return NodeType::IntegerLiteral; }
    int getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "IntegerLiteral: " << value_;
    }
};

class FloatLiteral : public ASTNode
{
private:
    float value_;

public:
    FloatLiteral(float value) : value_(value) {}
    NodeType getType() const override { return NodeType::FloatLiteral; }
    float getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FloatLiteral: " << value_;
    }
};

class StringLiteral : public ASTNode
{
private:
    std::string value_;

public:
    StringLiteral(std::string value) : value_(value) {}
    NodeType getType() const override { return NodeType::StringLiteral; }
    std::string getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StringLiteral: " << value_;
    }
};

class Identifier : public ASTNode
{
public:
    Identifier(std::string name) : name_(name) {}
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

class BinaryExpression : public ASTNode
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

    BinaryExpression(ASTNodePtr left, Operator op, ASTNodePtr right)
        : left_(std::move(left)), op_(op), right_(std::move(right)) {}

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

#endif // AST_H