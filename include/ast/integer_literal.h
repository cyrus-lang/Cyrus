#ifndef AST_INTEGER_LITERAL_H
#define AST_INTEGER_LITERAL_H

#include "ast.h"

struct IntegerLiteral_t {
    ASTNode base;
    int value;
};

IntegerLiteral* createIntegerLiteral(int value) {
    IntegerLiteral* int_literal = (IntegerLiteral*)malloc(sizeof(IntegerLiteral));
    if (!int_literal) {
        perror("Failed to allocate IntegerLiteral");
        exit(EXIT_FAILURE);
    }
    int_literal->base.type = INTEGER_LITERAL;
    int_literal->base.print = printIntegerLiteral;
    int_literal->value = value;
    return int_literal;
}

int getIntegerLiteralValue(const IntegerLiteral* literal) {
    return literal->value;
}

void printIntegerLiteral(const ASTNode* node, int indent) {
    const IntegerLiteral* literal = (const IntegerLiteral*)node;
    printIndent(indent);
    printf("IntegerLiteral: %d\n", literal->value);
}

#endif //AST_INTEGER_LITERAL_H
