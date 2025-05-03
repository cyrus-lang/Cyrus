#ifndef AST_PROGRAM_H
#define AST_PROGRAM_H

#include "ast.h"

struct Program_t
{
    ASTNode base;
    ASTNodeList *statements;
};

Program *createProgram(ASTNodeList *statements)
{
    Program *program = (Program *)malloc(sizeof(Program));
    if (!program)
    {
        perror("Failed to allocate memory for Program");
        exit(EXIT_FAILURE);
    }
    program->base.type = PROGRAM;
    program->base.print = printProgram;
    program->statements = statements;
    return program;
}

const ASTNodeList *getProgramStatements(const Program *program)
{
    return program->statements;
}

void printProgram(const ASTNode *node, int indent)
{
    const Program *program = (const Program *)node;
    printIndent(indent);
    printf("Program:\n");
    for (size_t i = 0; i < program->statements->size; ++i)
    {
        program->statements->data[i]->print(program->statements->data[i], indent + 1);
    }
}

#endif // AST_PROGRAM_H
