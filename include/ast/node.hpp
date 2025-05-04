#ifndef AST_NODE_HPP
#define AST_NODE_HPP

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
        TypeSpecifier,
        ImportStatement,
        FunctionDefinition,
        FunctionParameter,
        ASTVariableDeclaration
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

#endif //AST_NODE_HPP
