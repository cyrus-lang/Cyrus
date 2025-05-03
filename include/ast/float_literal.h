#ifndef AST_FLOAT_LITERAL
#define AST_FLOAT_LITERAL

#include "ast.h"

struct FloatLiteral_t {
    ASTNode base;
    float value;
};

FloatLiteral* createFloatLiteral(float value) {
    FloatLiteral* float_literal = (FloatLiteral*)malloc(sizeof(FloatLiteral));
    if (!float_literal) {
        perror("Failed to allocate FloatLiteral");
        exit(EXIT_FAILURE);
    }
    float_literal->base.type = FLOAT_LITERAL;
    float_literal->base.print = printFloatLiteral;
    float_literal->value = value;
    return float_literal;
}

float getFloatLiteralValue(const FloatLiteral* literal) {
    return literal->value;
}

void printFloatLiteral(const ASTNode* node, int indent) {
    const FloatLiteral* literal = (const FloatLiteral*)node;
    printIndent(indent);
    printf("FloatLiteral: %f\n", literal->value);
}


#endif //AST_FLOAT_LITERAL
