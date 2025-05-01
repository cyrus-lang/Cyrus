#ifndef PROGRAM_NODE_H
#define PROGRAM_NODE_H

#include "ASTNode.h"
#include <vector>
#include <iostream>
#include <string>

class ASTProgram : public ASTNode {
public:
    ASTProgram() : ASTNode(NodeType::Program) {}
    ~ASTProgram() override {} 

    void addStatement(ASTNode* statement) {
        addChild(statement);
    }

    const std::vector<ASTNode*>& getStatements() const {
        return getChildren();
    }

    void print(int indent = 0) const override {
        for (int i = 0; i < indent; ++i) {
            std::cout << "  ";
        }
        std::cout << "Program" << std::endl;
        for (const auto& statement : getChildren()) {
            statement->print(indent + 1);
        }
    }

protected:
    std::string getNodeTypeName() const override {
        return "Program";
    }
};

#endif // PROGRAM_NODE_H