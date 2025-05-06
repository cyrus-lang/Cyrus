#include "ast/ast.hpp"
#include "parser_test.hpp"

TEST(ParserExpressionTest, SimpleBinaryExpression)
{
    std::string input = "#my_var = 1 + 2;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
    ASSERT_EQ(program->getStatements().size(), 1);

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASSERT_EQ(varDecl->getType(), ASTNode::NodeType::VariableDeclaration);
    ASSERT_EQ(varDecl->getName(), "my_var");

    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getType(), ASTNode::NodeType::BinaryExpression);
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Add);

    ASTIntegerLiteral* left = static_cast<ASTIntegerLiteral*>(binaryExpr->getLeft());
    ASSERT_EQ(left->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(left->getValue(), 1);

    ASTIntegerLiteral* right = static_cast<ASTIntegerLiteral*>(binaryExpr->getRight());
    ASSERT_EQ(right->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(right->getValue(), 2);
}

TEST(ParserExpressionTest, BinaryExpressionWithSubtraction)
{
    std::string input = "#my_var = 5 - 3;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
    ASSERT_EQ(program->getStatements().size(), 1);

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASSERT_EQ(varDecl->getType(), ASTNode::NodeType::VariableDeclaration);
    ASSERT_EQ(varDecl->getName(), "my_var");

    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getType(), ASTNode::NodeType::BinaryExpression);
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Subtract);

    ASTIntegerLiteral* left = static_cast<ASTIntegerLiteral*>(binaryExpr->getLeft());
    ASSERT_EQ(left->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(left->getValue(), 5);

    ASTIntegerLiteral* right = static_cast<ASTIntegerLiteral*>(binaryExpr->getRight());
    ASSERT_EQ(right->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(right->getValue(), 3);
}

TEST(ParserExpressionTest, BinaryExpressionWithMultiplication)
{
    std::string input = "#my_var = 4 * 6;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
    ASSERT_EQ(program->getStatements().size(), 1);

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASSERT_EQ(varDecl->getType(), ASTNode::NodeType::VariableDeclaration);
    ASSERT_EQ(varDecl->getName(), "my_var");

    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getType(), ASTNode::NodeType::BinaryExpression);
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Multiply);

    ASTIntegerLiteral* left = static_cast<ASTIntegerLiteral*>(binaryExpr->getLeft());
    ASSERT_EQ(left->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(left->getValue(), 4);

    ASTIntegerLiteral* right = static_cast<ASTIntegerLiteral*>(binaryExpr->getRight());
    ASSERT_EQ(right->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(right->getValue(), 6);
}

TEST(ParserExpressionTest, BinaryExpressionWithDivision)
{
    std::string input = "#my_var = 10 / 2;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
    ASSERT_EQ(program->getStatements().size(), 1);

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASSERT_EQ(varDecl->getType(), ASTNode::NodeType::VariableDeclaration);
    ASSERT_EQ(varDecl->getName(), "my_var");

    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getType(), ASTNode::NodeType::BinaryExpression);
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Divide);

    ASTIntegerLiteral* left = static_cast<ASTIntegerLiteral*>(binaryExpr->getLeft());
    ASSERT_EQ(left->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(left->getValue(), 10);

    ASTIntegerLiteral* right = static_cast<ASTIntegerLiteral*>(binaryExpr->getRight());
    ASSERT_EQ(right->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(right->getValue(), 2);
}

TEST(ParserExpressionTest, BinaryExpressionWithRemainder)
{
    std::string input = "#my_var = 7 % 3;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
    ASSERT_EQ(program->getStatements().size(), 1);

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASSERT_EQ(varDecl->getType(), ASTNode::NodeType::VariableDeclaration);
    ASSERT_EQ(varDecl->getName(), "my_var");

    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getType(), ASTNode::NodeType::BinaryExpression);
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Remainder);

    ASTIntegerLiteral* left = static_cast<ASTIntegerLiteral*>(binaryExpr->getLeft());
    ASSERT_EQ(left->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(left->getValue(), 7);

    ASTIntegerLiteral* right = static_cast<ASTIntegerLiteral*>(binaryExpr->getRight());
    ASSERT_EQ(right->getType(), ASTNode::NodeType::IntegerLiteral);
    ASSERT_EQ(right->getValue(), 3);
}

TEST(ParserExpressionTest, BinaryExpressionWithEqual)
{
    std::string input = "#my_var = 5 == 5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Equal);
}

TEST(ParserExpressionTest, BinaryExpressionWithNotEqual)
{
    std::string input = "#my_var = 5 != 3;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::NotEqual);
}

TEST(ParserExpressionTest, BinaryExpressionWithLessThan)
{
    std::string input = "#my_var = 5 < 10;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::LessThan);
}

TEST(ParserExpressionTest, BinaryExpressionWithLessEqual)
{
    std::string input = "#my_var = 5 <= 5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::LessEqual);
}

TEST(ParserExpressionTest, BinaryExpressionWithGreaterThan)
{
    std::string input = "#my_var = 10 > 5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::GreaterThan);
}

TEST(ParserExpressionTest, BinaryExpressionWithGreaterEqual)
{
    std::string input = "#my_var = 10 >= 5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::GreaterEqual);
}

TEST(ParserExpressionTest, BinaryExpressionWithLogicalAnd)
{
    std::string input = "#my_var = true && false;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::LogicalAnd);
}

TEST(ParserExpressionTest, BinaryExpressionWithLogicalOr)
{
    std::string input = "#my_var = true || false;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::LogicalOr);
}

TEST(ParserExpressionTest, UnaryExpressionNegate)
{
    std::string input = "#my_var = -5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::Negate);

    ASTIntegerLiteral* operand = static_cast<ASTIntegerLiteral*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getValue(), 5);
}

TEST(ParserExpressionTest, UnaryExpressionLogicalNot)
{
    std::string input = "#my_var = !true;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::LogicalNot);
}

TEST(ParserExpressionTest, UnaryExpressionBitwiseNot)
{
    std::string input = "#my_var = ~10;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::BitwiseNot);

    ASTIntegerLiteral* operand = static_cast<ASTIntegerLiteral*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getValue(), 10);
}

TEST(ParserExpressionTest, UnaryExpressionPlus)
{
    std::string input = "#my_var = +5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::Plus);

    ASTIntegerLiteral* operand = static_cast<ASTIntegerLiteral*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getValue(), 5);
}

TEST(ParserExpressionTest, UnaryExpressionDereference)
{
    std::string input = "#my_var = *ptr;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::Dereference);

    ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "ptr");
}

TEST(ParserExpressionTest, UnaryExpressionPreIncrement)
{
    std::string input = "#my_var = ++i;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::PreIncrement);

    ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "i");
}

TEST(ParserExpressionTest, UnaryExpressionPreDecrement)
{
    std::string input = "#my_var = --j;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::PreDecrement);

    ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "j");
}

TEST(ParserExpressionTest, UnaryExpressionPostIncrement)
{
    std::string input = "#my_var = k++;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::PostIncrement);

   ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "k");
}

TEST(ParserExpressionTest, UnaryExpressionPostDecrement)
{
    std::string input = "#my_var = l--;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTUnaryExpression* unaryExpr = static_cast<ASTUnaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(unaryExpr->getOperator(), ASTUnaryExpression::Operator::PostDecrement);

   ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(unaryExpr->getOperand());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "l");
}

TEST(ParserExpressionTest, NestedBinaryExpression) {
    std::string input = "#my_var = (1 + 2) * 3;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* outerBinaryExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(outerBinaryExpr->getOperator(), ASTBinaryExpression::Operator::Multiply);

    ASTBinaryExpression* innerBinaryExpr = static_cast<ASTBinaryExpression*>(outerBinaryExpr->getLeft());
    ASSERT_EQ(innerBinaryExpr->getOperator(), ASTBinaryExpression::Operator::Add);

    ASTIntegerLiteral* left = static_cast<ASTIntegerLiteral*>(innerBinaryExpr->getLeft());
    ASSERT_EQ(left->getValue(), 1);

    ASTIntegerLiteral* right = static_cast<ASTIntegerLiteral*>(innerBinaryExpr->getRight());
    ASSERT_EQ(right->getValue(), 2);

    ASTIntegerLiteral* outerRight = static_cast<ASTIntegerLiteral*>(outerBinaryExpr->getRight());
    ASSERT_EQ(outerRight->getValue(), 3);
}

TEST(ParserExpressionTest, ComplexExpression) {
    std::string input = "#my_var = (1 + 2) * (3 - 4) / 5;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTBinaryExpression* divideExpr = static_cast<ASTBinaryExpression*>(varDecl->getInitializer());
    ASSERT_EQ(divideExpr->getOperator(), ASTBinaryExpression::Operator::Divide);

    ASTBinaryExpression* multiplyExpr = static_cast<ASTBinaryExpression*>(divideExpr->getLeft());
    ASSERT_EQ(multiplyExpr->getOperator(), ASTBinaryExpression::Operator::Multiply);

    ASTBinaryExpression* addExpr = static_cast<ASTBinaryExpression*>(multiplyExpr->getLeft());
    ASSERT_EQ(addExpr->getOperator(), ASTBinaryExpression::Operator::Add);

    ASTBinaryExpression* subExpr = static_cast<ASTBinaryExpression*>(multiplyExpr->getRight());
    ASSERT_EQ(subExpr->getOperator(), ASTBinaryExpression::Operator::Subtract);

    ASTIntegerLiteral* finalRight = static_cast<ASTIntegerLiteral*>(divideExpr->getRight());
    ASSERT_EQ(finalRight->getValue(), 5);
}

TEST(ParserExpressionTest, CastExpressionToFloat)
{
    std::string input = "#my_var = (float)my_int;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTCastExpression* castExpr = static_cast<ASTCastExpression*>(varDecl->getInitializer());
    ASSERT_EQ(castExpr->getType(), ASTNode::NodeType::CastExpression);
    ASSERT_EQ(castExpr->getTargetType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Float);

    ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(castExpr->getExpression());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "my_int");
}

TEST(ParserExpressionTest, NestedCastExpression)
{
    std::string input = "#my_var = (int)(float)my_double;";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTCastExpression* outerCastExpr = static_cast<ASTCastExpression*>(varDecl->getInitializer());
    ASSERT_EQ(outerCastExpr->getType(), ASTNode::NodeType::CastExpression);
    ASSERT_EQ(outerCastExpr->getTargetType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

    ASTCastExpression* innerCastExpr = static_cast<ASTCastExpression*>(outerCastExpr->getExpression());
    ASSERT_EQ(innerCastExpr->getType(), ASTNode::NodeType::CastExpression);
    ASSERT_EQ(innerCastExpr->getTargetType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Float);

    ASTImportedSymbolAccess* operand = static_cast<ASTImportedSymbolAccess*>(innerCastExpr->getExpression());
    ASSERT_EQ(operand->getSymbolPath().size(), 1);
    ASSERT_EQ(operand->getSymbolPath()[0], "my_double");
}

TEST(ParserExpressionTest, CastExpressionWithBinaryExpression)
{
    std::string input = "#my_var = (int)(my_float + 5);";
    ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

    ASTVariableDeclaration* varDecl = static_cast<ASTVariableDeclaration*>(program->getStatements()[0]);
    ASTCastExpression* castExpr = static_cast<ASTCastExpression*>(varDecl->getInitializer());
    ASSERT_EQ(castExpr->getType(), ASTNode::NodeType::CastExpression);
    ASSERT_EQ(castExpr->getTargetType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

    ASTBinaryExpression* binaryExpr = static_cast<ASTBinaryExpression*>(castExpr->getExpression());
    ASSERT_EQ(binaryExpr->getType(), ASTNode::NodeType::BinaryExpression);
    ASSERT_EQ(binaryExpr->getOperator(), ASTBinaryExpression::Operator::Add);
}