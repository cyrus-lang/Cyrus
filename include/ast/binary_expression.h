#ifndef AST_BINARY_EXPRESSION
#define AST_BINARY_EXPRESSION

#include "ast.h"

typedef enum
{
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    REMAINDER,
    EQUAL,
    NOT_EQUAL,
    LESS_THAN,
    LESS_EQUAL,
    GREATER_THAN,
    GREATER_EQUAL
} BinaryOperator;

struct BinaryExpression_t
{
    ASTNode base;
    BinaryOperator op;
    ASTNodePtr left;
    ASTNodePtr right;
};

BinaryExpression *createBinaryExpression(BinaryOperator op, ASTNodePtr left, ASTNodePtr right)
{
    BinaryExpression *bin_expr = (BinaryExpression *)malloc(sizeof(BinaryExpression));
    if (!bin_expr)
    {
        perror("Failed to allocate BinaryExpression");
        exit(EXIT_FAILURE);
    }
    bin_expr->base.type = BINARY_EXPRESSION;
    bin_expr->base.print = printBinaryExpression;
    bin_expr->op = op;
    bin_expr->left = left;
    bin_expr->right = right;
    return bin_expr;
}

const ASTNodePtr getBinaryExpressionLeft(const BinaryExpression *expr)
{
    return expr->left;
}

const ASTNodePtr getBinaryExpressionRight(const BinaryExpression *expr)
{
    return expr->right;
}

BinaryOperator getBinaryExpressionOp(const BinaryExpression *expr)
{
    return expr->op;
}

void printBinaryExpression(const ASTNode *node, int indent)
{
    const BinaryExpression *expr = (const BinaryExpression *)node;
    printIndent(indent);
    printf("BinaryExpression: ");

    switch (expr->op)
    {
    case ADD:
        printf("+\n");
        break;
    case SUBTRACT:
        printf("-\n");
        break;
    case MULTIPLY:
        printf("*\n");
        break;
    case DIVIDE:
        printf("/\n");
        break;
    case EQUAL:
        printf("==\n");
        break;
    case NOT_EQUAL:
        printf("!=\n");
        break;
    case LESS_THAN:
        printf("<\n");
        break;
    case LESS_EQUAL:
        printf("<=\n");
        break;
    case GREATER_THAN:
        printf(">\n");
        break;
    case GREATER_EQUAL:
        printf(">=\n");
        break;
    default:
        printf("Unknown\n");
        break;
    }

    expr->left->print(expr->left, indent + 1);
    expr->right->print(expr->right, indent + 1);
}

#endif // AST_BINARY_EXPRESSION
