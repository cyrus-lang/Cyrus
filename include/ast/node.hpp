#ifndef AST_NODE_HPP
#define AST_NODE_HPP

#include <nlohmann/json.hpp>
#include <iostream>
#include <vector>

class ASTNode
{
public:
    enum class NodeType
    {
        Program,
        StatementList,
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
        FunctionDeclaration,
        FunctionParameter,
        TypeDefStatement,
        StructDefinition,
        StructField,
        StructInitialization,
        ConditionalExpression,
        AssignmentExpression,
        ImportedSymbolAccess,
        FunctionCall,
        FieldAccess,
        PointerFieldAccess,
        EnumVariant,
        EnumDefinition,
        ReturnStatement,
        ContinueStatement,
        BreakStatement,
        ForStatement,
        IfStatement
    };

    virtual ~ASTNode() = default;
    virtual NodeType getType() const = 0;

    virtual void print(int) const = 0;
    virtual nlohmann::json jsonify() const {}

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
