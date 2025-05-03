#ifndef AST_STRING_LITERAL
#define AST_STRING_LITERAL

#include "ast.h"

struct StringLiteral_t {
    ASTNode base;
    char* value;
};

StringLiteral* createStringLiteral(const char* value) {
    StringLiteral* str_literal = (StringLiteral*)malloc(sizeof(StringLiteral));
    if (!str_literal) {
        perror("Failed to allocate StringLiteral");
        exit(EXIT_FAILURE);
    }
    str_literal->base.type = STRING_LITERAL;
    str_literal->base.print = printStringLiteral;
    str_literal->value = strdup(value);
    if (!str_literal->value) {
        perror("Failed to duplicate string");
        free(str_literal);
        exit(EXIT_FAILURE);
    }
    return str_literal;
}

const char* getStringLiteralValue(const StringLiteral* literal) {
    return literal->value;
}

void printStringLiteral(const ASTNode* node, int indent) {
    const StringLiteral* literal = (const StringLiteral*)node;
    printIndent(indent);
    printf("StringLiteral: %s\n", literal->value);
}

#endif //AST_STRING_LITERAL
