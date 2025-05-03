#ifndef AST_IDENTIFIER
#define AST_IDENTIFIER

#include "ast.h"

struct Identifier_t {
    ASTNode base;
    char* name;
};

Identifier* createIdentifier(const char* name) {
    Identifier* identifier = (Identifier*)malloc(sizeof(Identifier));
    if (!identifier) {
        perror("Failed to allocate Identifier");
        exit(EXIT_FAILURE);
    }
    identifier->base.type = IDENT; // Using the renamed enum member
    identifier->base.print = printIdentifier;
    identifier->name = strdup(name);
    if (!identifier->name) {
        perror("Failed to duplicate string");
        free(identifier);
        exit(EXIT_FAILURE);
    }
    return identifier;
}

const char* getIdentifierName(const Identifier* identifier) {
    return identifier->name;
}

void printIdentifier(const ASTNode* node, int indent) {
    const Identifier* identifier = (const Identifier*)node;
    printIndent(indent);
    printf("Identifier: %s\n", identifier->name);
}

#endif //AST_IDENTIFIER
