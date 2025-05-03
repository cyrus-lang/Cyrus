#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    PROGRAM,
    IMPORT_MODULE,
    VARIABLE_DECLARATION,
    BINARY_EXPRESSION,
    INTEGER_LITERAL,
    FLOAT_LITERAL,
    STRING_LITERAL,
    IDENT,
} NodeType;

typedef struct ASTNode_t ASTNode;
typedef struct Program_t Program;
typedef struct IntegerLiteral_t IntegerLiteral;
typedef struct FloatLiteral_t FloatLiteral;
typedef struct StringLiteral_t StringLiteral;
typedef struct Identifier_t Identifier;
typedef struct BinaryExpression_t BinaryExpression;

struct ASTNode_t {
    NodeType type;
    
    void (*print)(const ASTNode *node, int indent);
};

void printProgram(const ASTNode *node, int indent);
void printIntegerLiteral(const ASTNode *node, int indent);
void printFloatLiteral(const ASTNode *node, int indent);
void printStringLiteral(const ASTNode *node, int indent);
void printIdentifier(const ASTNode *node, int indent);
void printBinaryExpression(const ASTNode *node, int indent);
void printIndent(int indent);

typedef ASTNode* ASTNodePtr;
typedef struct {
    ASTNodePtr *data;
    size_t size;
    size_t capacity;
} ASTNodeList;

ASTNodeList* createASTNodeList() {
    ASTNodeList* list = (ASTNodeList*)malloc(sizeof(ASTNodeList));
    if (!list) {
        perror("Failed to allocate ASTNodeList");
        exit(EXIT_FAILURE);
    }
    list->data = NULL;
    list->size = 0;
    list->capacity = 0;
    return list;
}

void addASTNode(ASTNodeList* list, ASTNodePtr node) {
    if (list->size >= list->capacity) {
        list->capacity = list->capacity == 0 ? 10 : list->capacity * 2;
        ASTNodePtr* new_data = (ASTNodePtr*)realloc(list->data, list->capacity * sizeof(ASTNodePtr));
        if (!new_data) {
            perror("Failed to reallocate ASTNodeList");
            exit(EXIT_FAILURE);
        }
        list->data = new_data;
    }
    list->data[list->size++] = node;
}

void freeASTNodeList(ASTNodeList* list) {
    if (list) {
        for (size_t i = 0; i < list->size; ++i) {
            free(list->data[i]); 
        }
        free(list->data);
        free(list);
    }
}

void printIndent(int indent) {
    for (int i = 0; i < indent; ++i) {
        printf("  ");
    }
}

#include "program.h"
#include "identifier.h"
#include "integer_literal.h"
#include "float_literal.h"
#include "string_literal.h"
#include "binary_expression.h"

#endif // AST_H