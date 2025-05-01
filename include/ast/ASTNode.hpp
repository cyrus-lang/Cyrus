#ifndef AST_NODE_H
#define AST_NODE_H

#include <iostream>
#include <string>
#include <vector>

class ASTNode {
public:
    enum NodeType {
        Program,
        Statement,
        Expression,
        Identifier,
        NumericLiteral,
        BinaryOp,
    };

protected:
    NodeType nodeType;
    std::vector<ASTNode*> children;

public:
    ASTNode(NodeType type) : nodeType(type) {}
    virtual ~ASTNode() {
        for (ASTNode* child : children) {
            delete child;
        }

        children.clear();
    }

    NodeType getType() const {
        return nodeType;
    }

    void addChild(ASTNode* child) {
        if (child != nullptr) {
            children.push_back(child);
        }
    }

    const std::vector<ASTNode*>& getChildren() const {
        return children;
    }

    virtual void print(int indent = 0) const {
        for (int i = 0; i < indent; ++i) {
            std::cout << "  ";
        }
        std::cout << "ASTNode (" << getNodeTypeName() << ")" << std::endl;
        for (const auto& child : children) {
            child->print(indent + 1);
        }
    }

protected:
    virtual std::string getNodeTypeName() const {
        switch (nodeType) {
            case Program:        return "Program";
            case Statement:      return "Statement";
            case Expression:     return "Expression";
            case Identifier:     return "Identifier";
            case NumericLiteral: return "NumericLiteral";
            case BinaryOp:       return "BinaryOp";
            default:             return "Unknown";
        }
    }
};

#endif // AST_NODE_H