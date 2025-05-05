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
        StatementList,
        ImportModule,
        VariableDeclaration,
        IntegerLiteral,
        FloatLiteral,
        StringLiteral,
        Identifier,
        CastExpression,
        BinaryExpression,
        UnaryExpression,
        TypeSpecifier,
        ImportStatement,
        FunctionDefinition,
        FunctionParameter,
        ASTVariableDeclaration,
        TypeDefStatement,
        StructDefinition,
        StructField,
        ConditionalExpression,
        AssignmentExpression,
        ImportedSymbolAccess,
        FunctionCall,
        FieldAccess,
        PointerFieldAccess
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
