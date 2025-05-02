#ifndef AST_H
#define AST_H

#include <iostream>
#include <vector>

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

using ASTNodePtr = std::unique_ptr<ASTNode>;
using ASTNodeList = std::vector<ASTNodePtr>;

class Program : public ASTNode
{
private:
    ASTNodeList statements_;

public:
    Program(ASTNodeList statements) : statements_(std::move(statements)) {}
    NodeType getType() const override { return NodeType::Program; }
    const ASTNodeList &getStatements() const { return statements_; }

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
    Identifier(std::string name) : name_(std::move(name)) {}
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
        GreaterEqual
    };

    BinaryExpression(Operator op, ASTNodePtr left, ASTNodePtr right) : op_(op), left_(std::move(left)), right_(std::move(right)) {}
    NodeType getType() const override { return NodeType::BinaryExpression; }
    const ASTNodePtr &getLeft() const { return left_; };
    const ASTNodePtr &getRight() const { return right_; };
    Operator getOp() const { return op_; };

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "BinaryExpression: " << std::endl;

        switch (op_)
        {
        case Operator::Add:
            std::cout << "+";
            break;
        case Operator::Subtract:
            std::cout << "-";
            break;
        case Operator::Multiply:
            std::cout << "*";
            break;
        case Operator::Divide:
            std::cout << "/";
            break;
        case Operator::Equal:
            std::cout << "==";
            break;
        case Operator::NotEqual:
            std::cout << "!=";
            break;
        case Operator::LessThan:
            std::cout << "<";
            break;
        case Operator::LessEqual:
            std::cout << "<=";
            break;
        case Operator::GreaterThan:
            std::cout << ">";
            break;
        case Operator::GreaterEqual:
            std::cout << ">=";
            break;
        default:
            std::cout << "Unknown";
            break;
        }

        left_->print(indent + 1);
        right_->print(indent + 1);
    }

private:
    Operator op_;
    ASTNodePtr left_;
    ASTNodePtr right_;
};

#endif // AST_H